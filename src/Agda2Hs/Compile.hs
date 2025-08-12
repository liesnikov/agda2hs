module Agda2Hs.Compile where

import Prelude hiding (null)

import Control.Monad.IO.Class
import Control.Monad.Trans.RWS.CPS ( evalRWST )
import Control.Monad.State ( gets, liftIO )
import Control.Arrow ((>>>))
import Data.Functor
import Data.IORef
import Data.List ( isPrefixOf, group, sort )

import qualified Data.Map as M

import Agda.Compiler.Backend
import Agda.Compiler.Common ( curIF )
import Agda.Utils.FileName ( isNewerThan )
import Agda.Syntax.TopLevelModuleName ( TopLevelModuleName )
import Agda.Syntax.Common.Pretty ( prettyShow )
import Agda.TypeChecking.Pretty
import Agda.TypeChecking.Monad.Signature ( isInlineFun )
import Agda.Utils.Impossible
import Agda.Utils.List
import Agda.Utils.Null
import Agda.Utils.Monad ( whenM, anyM, when, unless )

import Agda2Hs.Compile.ClassInstance ( compileInstance )
import Agda2Hs.Compile.Data ( compileData, checkCompileToDataPragma )
import Agda2Hs.Compile.Function ( compileFun, checkTransparentPragma, checkInlinePragma, checkCompileToFunctionPragma )
import Agda2Hs.Compile.Name ( hsTopLevelModuleName )
import Agda2Hs.Compile.Postulate ( compilePostulate )
import Agda2Hs.Compile.Record ( compileRecord, checkUnboxPragma, checkTuplePragma )
import Agda2Hs.Compile.RuntimeCheckUtils ( importDec, showOptions )
import Agda2Hs.Compile.Types
import Agda2Hs.Compile.Utils
import Agda2Hs.Config
import Agda2Hs.Pragma

import qualified Agda2Hs.Language.Haskell as Hs

globalSetup :: Options -> TCM GlobalEnv
globalSetup opts = do
  opts <- checkConfig opts
  ctMap <- liftIO $ newIORef M.empty
  return $ GlobalEnv opts ctMap

initCompileEnv :: GlobalEnv -> TopLevelModuleName -> Bool -> SpecialRules -> CompileEnv
initCompileEnv genv tlm rtc rewrites = CompileEnv
  { globalEnv         = genv
  , currModule        = tlm
  , minRecordName     = Nothing
  , isNestedInType    = False
  , locals            = []
  , compilingLocal    = False
  , whereModules      = []
  , copatternsEnabled = False
  , rtc               = rtc
  , rewrites          = rewrites
  , writeImports      = True
  , checkNames        = True
  }

initCompileState :: CompileState
initCompileState = CompileState { lcaseUsed = 0 }

runC :: GlobalEnv -> TopLevelModuleName -> Bool -> SpecialRules -> C a -> TCM (a, CompileOutput)
runC genv tlm rtc rewrites c = evalRWST c (initCompileEnv genv tlm rtc rewrites) initCompileState

moduleSetup :: GlobalEnv -> IsMain -> TopLevelModuleName -> Maybe FilePath -> TCM (Recompile ModuleEnv ModuleRes)
moduleSetup genv _ m mifile = do
  let opts = globalOptions genv
  -- we never compile primitive modules
  if any (`isPrefixOf` prettyShow m) primModules then pure $ Skip ()
  else do
    -- check whether the file needs to be recompiled
    uptodate <- case mifile of
      Nothing -> pure False
      Just ifile -> let ofile = moduleFileName opts m in
        liftIO =<< isNewerThan <$> ofile <*> pure ifile
    if uptodate then do
      reportSDoc "agda2hs.compile" 3 $ text "Module " <+> prettyTCM m <+> text " is already up-to-date"
      return $ Skip ()
    else do
      reportSDoc "agda2hs.compile" 3 $ text "Compiling module: " <+> prettyTCM m
      setScope . iInsideScope =<< curIF
      return $ Recompile m

-- Main compile function
------------------------

compile
  :: GlobalEnv -> ModuleEnv -> IsMain -> Definition
  -> TCM (RtcDefs, CompileOutput)
compile genv tlm _ def = do
  withCurrentModule (qnameModule qname)
    $ runC genv tlm rtc (optRewrites opts)
    $ setCurrentRangeQ qname
    $ (when rtc (liftTCM importDec)) *> compileAndTag <* postCompile
  where
    opts = globalOptions genv
    qname = defName def
    rtc = optRtc opts

    tag []   = []
    tag code = [(nameBindingSite $ qnameName qname, code)]

    compileAndTag :: C RtcDefs
    compileAndTag = (tag <$>) <$> do
      p <- processPragma qname
      pure $ setCommandLineOptions . stPersistentOptions . stPersistentState =<< getTC
      currentOptions <- useTC stPragmaOptions
      reportSDoc "agda2hs.compile.import" 25 $ "Current options are" <+> showOptions currentOptions
      reportSDoc "agda2hs.compile" 5  $ text "Compiling definition:" <+> prettyTCM qname
      reportSDoc "agda2hs.compile" 45 $ text "Pragma:" <+> text (show p)
      reportSDoc "agda2hs.compile" 65 $ text "Compiling definition:" <+> pretty (theDef def)

      isInstance <- anyM (isClassName . instanceClass) $ defInstance def

      reportSDoc "agda2hs.compile" 15  $ text "Is instance?" <+> prettyTCM isInstance

      --case (p , theDef def) of
      --  (NoPragma            , _         ) -> cnil
      --  (ExistingClassPragma , _         ) -> cnil
      --  (UnboxPragma s       , Record{}  ) -> cnil <* checkUnboxPragma def
      --  (TransparentPragma   , Function{}) -> cnil <* checkTransparentPragma def
      --  (InlinePragma        , Function{}) -> cnil <* checkInlinePragma def
      --  (TuplePragma b       , Record{}  ) -> cnil
      --  (CompileToPragma s   , Datatype{}) -> cnil <* checkCompileToDataPragma def s
      --  (CompileToPragma s   , Function{}) -> cnil <* checkCompileToFunctionPragma def s
      --  (ClassPragma ms      , Record{}  ) -> cone $ compileRecord (ToClass ms) def
      --  (NewTypePragma ds    , Record{}  ) -> cone $ compileRecord (ToRecord True ds) def
      --  (NewTypePragma ds    , Datatype{}) -> compileData True ds def
      --  (DefaultPragma ds    , Datatype{}) -> compileData False ds def
      --  (DerivePragma s      , _         ) | isInstance -> cone $ compileInstance (ToDerivation s) def
      --  (DefaultPragma _     , Axiom{}   ) | isInstance -> cone $ compileInstance (ToDerivation Nothing) def
      --  (DefaultPragma _     , _         ) | isInstance -> cone $ compileInstance ToDefinition def
      --  (DefaultPragma _     , Axiom{}   ) -> compilePostulate def
      --  (DefaultPragma _     , Function{}) -> compileFun True def
      --  (DefaultPragma ds    , Record{}  ) -> cone $ compileRecord (ToRecord False ds) def
      --
      --  _ -> agda2hsErrorM $ text "Don't know how to compile" <+> prettyTCM (defName def)

      case (p , theDef def) of
        (NoPragma           , _          ) -> return $ WithRtc [] []
        (ExistingClassPragma, _          ) -> return $ WithRtc [] []
        (DefaultPragma _    , Function {}) | not isInstance -> compileFun True def
        (NewTypePragma ds   , Datatype {}) -> compileData True ds def
        (DefaultPragma ds   , Datatype {}) -> compileData False ds def
        (ClassPragma ms     , Record {}  ) -> compileRecord (ToClass ms) def
        (NewTypePragma ds   , Record {}  ) -> compileRecord (ToRecord True ds) def
        (DefaultPragma ds   , Record {}  ) | not isInstance -> compileRecord (ToRecord False ds) def
        -- ^ Names that may induce runtime checks or are safe to have none
        _ -> do
          tellNoErased $ prettyShow $ qnameName $ defName def
          (`WithRtc` []) <$> case (p, theDef def) of
            (UnboxPragma s    , Record {}  ) -> [] <$ checkUnboxPragma def
            (TuplePragma b    , Record{}   ) -> [] <$ checkTuplePragma def
            (TransparentPragma, Function {}) -> [] <$ checkTransparentPragma def
            (InlinePragma     , Function {}) -> [] <$ checkInlinePragma def
            (DerivePragma s   , _          ) | isInstance -> pure <$> compileInstance (ToDerivation s) def
            (DefaultPragma _  , Axiom {}   ) | isInstance -> pure <$> compileInstance (ToDerivation Nothing) def
            (DefaultPragma _  , _          ) | isInstance -> pure <$> compileInstance ToDefinition def
            (DefaultPragma _  , Axiom {}   ) -> compilePostulate def
            -- FIXME: this probably isn't right wrt rtc
            (CompileToPragma s   , Datatype{}) -> cnil <* checkCompileToDataPragma def s
            (CompileToPragma s   , Function{}) -> cnil <* checkCompileToFunctionPragma def s
            _ -> agda2hsErrorM $ text "Don't know how to compile" <+> prettyTCM (defName def)
    postCompile :: C ()
    postCompile = whenM (gets $ lcaseUsed >>> (> 0)) $ tellExtension Hs.LambdaCase

verifyOutput ::
  GlobalEnv -> ModuleEnv -> IsMain -> TopLevelModuleName
  -> [(RtcDefs, CompileOutput)] -> TCM ()
verifyOutput _ _ _ m ls = do
  reportSDoc "agda2hs.compile" 5 $ text "Checking generated output before rendering: " <+> prettyTCM m
  ensureUniqueConstructors
  ensureNoOutputFromHsModules
  where
    ensureUniqueConstructors = do
      let allCons = do
            -- take from concat'd definitions and runtime checks
            r <- ls >>= (\(WithRtc d r) -> d : [r]) . fst
            (_, a) <- r
            Hs.DataDecl _ _ _ _ cons _ <- a
            Hs.QualConDecl _ _ _ con <- cons
            return $ case con of
              Hs.ConDecl _ n _ -> n
              Hs.InfixConDecl _ _ n _ -> n
              Hs.RecDecl _ n _ -> n
          duplicateCons = filter ((> 1) . length) . group . sort  $ allCons
      when (length duplicateCons > 0) $
        agda2hsErrorM $ vcat (map (\x -> text $ "Cannot generate multiple constructors with the same identifier: " <> Hs.prettyPrint (headWithDefault __IMPOSSIBLE__ x)) duplicateCons)

    ensureNoOutputFromHsModules = unless (all (null . getAllRtc . fst) ls) $ do
      let hsModName = hsTopLevelModuleName m
      case hsModuleKind hsModName of
        HsModule -> do
          reportSDoc "agda2hs.compile" 10 $ text "Haskell module" <+> prettyTCM m <+> text "has non-null output."
          agda2hsErrorM $ hsep
            (  pwords "The `Haskell.` namespace are reserved for binding existing Haskell modules, so the module"
            ++ [text "`" <> prettyTCM m <> text "`"]
            ++ pwords "should not contain any"
            ++ [text "`{-# COMPILE AGDA2HS ... #-}`"]
            ++ pwords "pragmas that produce Haskell code."
            )
        PrimModule -> __IMPOSSIBLE__
        AgdaModule -> return ()
