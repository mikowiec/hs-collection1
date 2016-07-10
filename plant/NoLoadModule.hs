

module NoLoadModule where

import qualified LSystem
--import qualified Plants1
import qualified Simple
-- import qualified Plants2
import qualified Plants2
--import qualified Gran

import SimEngine
import GeomRep
import qualified Misc

load_obj mod = return (Just True)

unload mod = return True

type Res = (String,String)
type DelRet = Maybe (Either Res Res)

init :: IO [IO (String, Maybe (IO (Maybe (Either Res Res))))]
init = return []

init_fg :: IO [IO (String, Maybe (IO (Maybe (Either Res Res))))]
init_fg = NoLoadModule.init

--compile_fg :: String -> String -> IO (IO (DelRet))
compile_fg hs obj = compile hs obj

compile :: String -> String -> IO (IO (Maybe (Either Res Res)))
compile hs obj = return (return (Just (Right ("ok",""))))

load_mod _ = return (Just ())


dbg =
 SimEngine $ SimEngineInst
    (\_ -> Geometry [])
    (\_ _ -> Geometry [])
    (\_ _ -> [])
    (\s _ _ -> s)
    (\_ -> 0)
    (\_ -> (0,0,0))
    (SimEnv ())
    (0,())
    (0,())
    (\(age,t) _ -> ((succ age :: Int, t)))
    (\_ -> StrTree 0 "" [])
    (\_ -> "<none>")


load_sym :: t -> String -> IO (IO [(String, SimEngine)])
load_sym _ sym = do
    return $ do
        xs <- return [] -- Plants2.mk_plants
        ys <- LSystem.mk_plants
        ts <- return [] -- Plants1.mk_plants
        ss <- Simple.mk_plants
        let ds = [("debug", dbg)]
        return (ds ++ ys ++ xs ++ ts ++ ss)

