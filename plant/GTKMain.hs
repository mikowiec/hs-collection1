

module GTKMain where

import Prelude hiding (init)

import FiniteMap
import Maybe
import Monad
import System (getArgs, exitWith, ExitCode(..))
import Time
import Types
import PlantTypes
import Directory
import Povray

import List
import IO

import Gdk hiding (Point)
import Gtk hiding (main)
import qualified Gtk(main)
import qualified Glade

import GdkGL
import GtkGL
import Random
import Misc

import IOExts
import Texture
import LinAlg
import Profiling

import Pf

import GLView
import GLMisc


import GuiParts
import GeomRep

import System
import SimEngine
import Property
import DeepSeq

import qualified NoLoadModule as LM


data GuiConf = GuiConf {
    disp_box    :: Bool,
    disp_ground :: Bool,
    disp_target :: Bool,
    disp_coords :: Bool,
    texturing :: Bool,
    camera_pos :: Point,
    camera_ori :: Quat,
    camera_tgt :: Point,
    cur_model_file  :: Maybe String,
    cur_engine_name :: Maybe String
  } deriving (Read, Show)


data GuiState = GuiState {
    sim_engines     :: [(String, SimEngine)],
    model_file_hdr  :: Maybe String,
    cur_engine      :: Maybe SimEngine,
    geom_rep        :: Maybe ProcGeomRep,
    textures        :: Textures,
    gui_conf       :: GuiConf,
    strtree_state   :: FiniteMap [Int] (Int,Bool),
    selected_node   :: Maybe (Int, [(String,PropType,EntryBox)]),
    selected_node_wds :: [Widget],
    pick_geoms      :: [(Int, (Vector,Float))],
    redisplay :: (?gui_ref::IORef GuiState) => GuiConf -> IO ()
  }

data EntryBox = EntryBox HBox Label Entry

gui_var p = p $^ readIORef ?gui_ref
gui_upd f = modifyIORef ?gui_ref f

gfx_size :: ProcGeomRep -> (Int,Int)
gfx_size (Left _) = (0, 0)
gfx_size (Right rep') = (num_lines rep', num_leaves rep')
 where num_lines (_,_,n,_) = n
       num_leaves (_,_,_,n) = n


gl_ok wd ret gl = do
    allowed <- areaMakeCurrent wd
    when allowed gl
    return ret

gl_init wd  = gl_ok wd () $ do
    ts <- texInit ["leaf1","tuja", "leaf_brown", "leaf_red", "leaf_yellow", "leaf1faded"]
    init_gl
    modifyIORef ?gui_ref (\st -> st { textures = ts })
    gl_reshape wd ()
    return ()

avg x y = (x+y)/2

avg_pt (x1,x2,x3) (y1,y2,y3) = (avg x1 y1, avg x2 y2, avg x3 y3)

--add_sph _ [] ps = ps
add_sph ix gs ps = foldr g ps gs -- addToFM ps (foldr1 avg_pt (map f gs)) ix
 where g gp ps = ((f gp), ix):ps
       f (Li _ _ p1 p2) = avg_pt p1 p2
       f (Pt p _) = p
       f (GeomRep.Branch _ _ _ p1 p2) = avg_pt p1 p2
       f (GeomRep.Square _ s) = (0,s/2,0)
       f (PtA p _ _ _) = p
       f (FnBranch _ (x:_) (Fn f)) = snd (f x)

calc_bsphere p o _ g = (p `add` (v `rot_vec_q` o) , d)
 where (v,d) = case g of
        GeomRep.Li _ _ p1 p2 -> (avg_pt p1 p2, 0.2)
        GeomRep.LiA w1 w2 p1 p2 _ _ -> (avg_pt p1 p2, avg w1 w2)
        GeomRep.Pt p v -> (p, absval v)
        GeomRep.PtA p _ s _ -> (p, s)
        GeomRep.Branch _ w1 w2 p1 p2 -> (avg_pt p1 p2, avg w1 w2)
        GeomRep.Square su sz -> ((0,0,0), sz)


map_loc f (LocGeom v o s i gp) = map (\g -> (i, f v o s g)) gp

eng_draw eng rep ts dc = do
{-
    ps <- gui_var pick_geoms
    let ((ps',s_rep),eng') = se_fold_geom eng ([],GLView.acc_init) acc_fn
        acc_fn ix gs (ps,dr) = (add_sph ix gs ps, GLView.acc_geom gs dr)
        (ps'',new_rep) = maybe (ps',Left s_rep) ((,) ps . Right) rep
-}

    let new_rep = maybe (Left (se_get_geom2 eng)) id rep

    pm_adj <- rangeGetAdjustment (range $ interpolate_slider ?gui)
    pm <- adjustmentGetValue pm_adj

    wd_adj <- rangeGetAdjustment (range $ wind_slider ?gui)
    wd <- adjustmentGetValue wd_adj

    let param = (pm, (wd,-1,0))

    (rep') <- GLView.draw param new_rep (if dc.!texturing then ts else [])
    when (dc.!disp_target) $ case new_rep of
        Right _ -> return ()
        Left rep -> do
            let lg = flatten (apply_param param rep)
            let ps = concatMap (map_loc calc_bsphere) lg
            modifyIORef ?gui_ref (\g -> g { pick_geoms = ps })

    when (dc.!disp_target) $
        readIORef ?gui_ref >>= mapM_ (draw_target.fst.snd) . pick_geoms

--    when (dc.!disp_target) $ draw_target (dc.!camera_tgt)
    return (Just (Right rep'))
    


gl_draw wd _  = gl_ok wd True $ do
    clear_view
    gs <- readIORef ?gui_ref
    let (ms,dc,rep) = (cur_engine gs, gui_conf gs, geom_rep gs)
    ms <- cur_engine $^ readIORef ?gui_ref
    setup_view (dc.!camera_pos) (dc.!camera_ori) (dc.!camera_tgt)
    when (dc.!disp_ground) $ draw_ground
    ifM ms $ \ss -> do
        rep' <- eng_draw ss rep (gs.!textures) dc
        modifyIORef ?gui_ref (\gs -> gs { geom_rep = rep' })
    when (dc.!disp_coords) $ draw_coords (dc.!camera_tgt)
    when (dc.!disp_box)    $ draw_grid 10
    areaSwapbuffers wd

gl_reshape wd _  = gl_ok wd True $ do
    (a, b, width, height) <- widgetGetAllocation wd
    init_view (fromIntegral width) (fromIntegral height)
    gl_draw wd ()
    return ()


gl_mouse _ _ = do 
    return True


gui_connect = Glade.xmlSignalConnect

get_age :: (?gui_ref :: IORef GuiState) => IO Int
get_age = (maybe 0 se_get_age . cur_engine) $^ readIORef ?gui_ref


data GLViewTransform = Press Float Float Int | Release Int | Motion Float Float

rot_with_left q dc  = dc { camera_ori = q `mulq` (dc.!camera_ori) }
rot_with_right q dc = dc { camera_ori = (dc.!camera_ori) `mulq` q }

glview_transform rref (Press x y btn) = do
    modifyIORef rref (\(g,b,_,_) -> (True,btn,x,y)) >> return False
glview_transform rref (Release btn) = do
    modifyIORef rref (\(g,b,x,y) -> (False,btn,x,y)) >> return False
glview_transform rref (Motion nx ny) = do
    (grab,b,x,y) <- readIORef rref
    if not grab then return False
     else do
           writeIORef rref (grab,b,nx,ny)
           let (dx,dy) = (nx-x,ny-y)
           let (r,fn) = case b of
                         1 -> (True, rot_with_right (rot_axis (0,1,0) (dx/100))
                                    . rot_with_left (rot_axis (1,0,0) (dy/100)))
                         2 -> (True, \dc -> let tgt = dc.!camera_tgt
                                                ori = inv $ dc.!camera_ori
                                                e_x = rot_vec_q (1,0,0) ori `emul` (1,0,1)
                                                e_z = rot_vec_q (0,0,1) ori `emul` (1,0,1)
                                                scale = (* (-0.1)) . realToFrac
                                                diff = (scale dx `mulk` e_x) `add`
                                                       (scale dy `mulk` e_z)
                                            in dc { camera_tgt = tgt `add` diff } )
                         3 -> (True, \dc -> let (x,y,z) = dc.!camera_pos
                                            in dc { camera_pos =
                                                (x,y, max 0.2 $ z-(0.1*(realToFrac dy)) )})
                         _ -> (False, id)
           modifyIORef ?gui_ref (\gs -> gs { gui_conf = fn (gs.!gui_conf) })
           return r

get_btn_pos btn = do
    x <- realToFrac $^ eventButtonGetX btn
    y <- realToFrac $^ eventButtonGetY btn
    return (x,y)



draw_struct tree eng = do
    treeClearItems tree 0 (-1)
    let st = simrep_strtree eng
    fold [] tree st
 where fold i t (StrTree ix s cs) = do
        it <- draw_node t i (show s)
        when (not (null cs)) (draw_children ix fold i cs it)
        return ()

       draw_node rt i str = do
            item <- treeItemNewWithLabel str
            treeAppend rt item
            widgetShowAll item
            return item

       draw_children ix f i cs' item = do
            sub <- treeNew
            treeItemSetSubtree item sub
            let add_children = mapM_ ((\(j,s) -> f (j:i) sub s)) (zip [0..] cs')
            xst <- strtree_state $^ readIORef ?gui_ref
            case lookupWithDefaultFM xst (0,False) i of
             (_,True) -> do add_children; treeItemExpand item
             (_,False) -> treeItemCollapse item
            signalConnect item $ TreeItemExpandHandler $ const $ do
                    modifyIORef ?gui_ref (\gs -> gs { strtree_state = addToFM (gs.!strtree_state) i (ix,True) })
                    add_children
                    widgetShowAll sub
            signalConnect item $ TreeItemCollapseHandler $ const $
                modifyIORef ?gui_ref (\gs -> gs { strtree_state = addToFM (gs.!strtree_state) i (ix,False)})
            return ()

connect_toggle tgl f = do
    signalConnect tgl $ ToggleButtonToggledHandler $ \btn -> do
        st <- toggleButtonQueryState btn
        f st
        return ()

connect_toggle_upd tgl f = do
    connect_toggle tgl (\st -> f st >> update False ())

update ch r = do
        dc <- gui_conf $^ readIORef ?gui_ref
        when ch $ do
            ms <- cur_engine $^ readIORef ?gui_ref
            ifM_ ms $ \ss -> do
                let (n,l,s) = se_get_size ss
                labelSetText (struct_size_nodes ?gui) (show n)
                labelSetText (struct_size_leaves ?gui) (show l)
                labelSetText (struct_sharing ?gui) (show s)
                draw_struct (struct_tree ?gui) ss
        disp <- gui_var redisplay
        dt <- profile_ $ disp dc
--        labelSetText (?gui.!profile_time_draw) (show dt)
        when ch $ do
            rep <- geom_rep $^ readIORef ?gui_ref
            let (ln,ls) = maybe (0,0) gfx_size rep
            labelSetText (draw_lines ?gui) (show ln)
            labelSetText (draw_leaves ?gui) (show ls)
        return r

reset_geometry :: (?gui_ref :: IORef GuiState) => IO ()
reset_geometry = do
    modifyIORef ?gui_ref $ \gs -> gs { geom_rep = Nothing }

simulate :: (?gui_ref :: IORef GuiState) => IO ()
simulate =
    modifyIORef ?gui_ref $ \gs ->
        let eng = fmap se_simulate_step (cur_engine gs)
        in gs { cur_engine = deepSeq eng eng, 
                geom_rep = Nothing }


dummy_simul :: (?gui :: Gui, ?gui_ref :: IORef GuiState) => IO ()
dummy_simul = do
    modifyIORef ?gui_ref (\gs -> gs { sim_engines = [],
                                      cur_engine = Nothing,
                                      geom_rep = Nothing })
    update True ()


connect_button btn m = signalConnect btn $ ButtonClickedHandler $ const m

basename = reverse . takeWhile (/='/') . reverse


real_pos :: Vector -> Vector -> Quat -> Vector
real_pos pos tgt ori = (rot_vec_q pos (inv ori)) `add` tgt


with_geomrep f = do
    mf <- gui_var (cur_model_file.gui_conf)
    lbl <- gui_var (cur_engine_name.gui_conf)
    eng <- gui_var cur_engine
    case (mf,lbl,eng) of
     (Just mfn, Just lbl, Just eng) -> f mfn lbl (se_get_geom eng) >> return ()
     _ -> return ()
    

export_pov :: (?gui_ref :: IORef GuiState) => IO ()
export_pov = with_geomrep $ \mfn lbl gr -> do
        dc <- gui_var gui_conf
        let rep = flatten (apply_param (0,(0,-1,0)) gr)
        let fn = (remove_ext mfn) ++ "_" ++ lbl ++ ".pov"
        let pos = real_pos (camera_pos dc) (camera_tgt dc) (camera_ori dc)
        writeFile fn ((geom2povray pos (inv (camera_ori dc)) rep))
        let (w,h) = (800, 600)
        system (sprintf ("povray +A +FN +SP16 +W"&" +H"&" +D +P +I"&." &") w h fn)
        
export_flat_geomrep :: (?gui_ref :: IORef GuiState) => IO ()
export_flat_geomrep = with_geomrep $ 
    \mfn lbl gr -> do
        let fn = (remove_ext mfn) ++ "_" ++ lbl ++ ".geom"
        writeFile fn (show (flatten (apply_param (0,(0,-1,0)) gr)))
        return ()


export_geomrep :: (?gui_ref :: IORef GuiState) => IO ()
export_geomrep = with_geomrep $ 
    \mfn lbl gr -> do
        let fn = (remove_ext mfn) ++ "_" ++ lbl ++ ".geom"
        writeFile fn (show (apply_param (0,(0,-1,0)) gr))
        return ()

    
import_geomrep :: (?gui :: Gui, ?gui_ref :: IORef GuiState) => IO ()
import_geomrep = do
    fs <- fileSelectionNew "Load Geomrep"
    (ok,cancel) <- fileSelectionQueryButtons fs
    widgetShow fs
    let close = widgetDestroy (widget fs)
    connect_button cancel close
    connect_button ok $ do
        fn <- fileSelectionGetFilename fs
        s <- readFile fn
        gr <- readIO s `catch` (\_-> unflatten $^ readIO s)
        modifyIORef ?gui_ref (\g -> g { geom_rep = Just (Left gr) })
        close
        update False ()
    return ()

open_dialog reload = do
    fs <- fileSelectionNew "Load Plant Models"
    (ok,cancel) <- fileSelectionQueryButtons fs
    widgetShow fs
    let close = widgetDestroy (widget fs)
    connect_button ok (do fn <- fileSelectionGetFilename fs
                          let f gc = gc { cur_model_file = Just fn }
                          upd_dc f
                          reload
                          close)
    connect_button cancel close
    return ()

pairify xs = zipWith (,) xs (tail xs)

readM :: Read a => String -> Maybe a
readM = fmap fst . listToMaybe . reads

read_pair c xs =
    case break (==c) xs of
     (as,(_:bs)) -> case (readM as, readM bs) of
                     (Just a, Just b) -> Just (a,b)
                     _ -> Nothing
     _ -> Nothing


dist lp1 llen sp1 = 
    let sr = sp1 `sub` lp1
        e_l = norm llen
        sd = sr `sub` ((dot sr e_l) `mulk` e_l)
    in sd

intersect_line_sphere lp1 llen (sp1,ix) =
    absval (dist lp1 llen sp1) < 0.1


dc_to_gui dc = do
    let pos = dc.!camera_pos
    let ori = dc.!camera_ori
    let tgt = dc.!camera_tgt
    let cpos = real_pos pos tgt ori
    let gui = ?gui
    labelSetText (gui.!camera_position_label) (pt_fmt cpos)
    labelSetText (gui.!camera_orientation_label) (q_fmt ori)
    labelSetText (gui.!camera_target_label) (pt_fmt tgt)
    mapM_ (\(b,s) -> toggleButtonSetActive (gui.!b) (dc.!s)) $
        (toggle_draw_box, disp_box) :
        (toggle_draw_ground, disp_ground) :
        (toggle_draw_coords, disp_coords) :
        (toggle_mark_targets, disp_target) :
        (toggle_texturing, texturing) :
        []



expand_path xst ix = 
    let xs = fmToList xst in
    case find (\(_,(ix',_)) -> dbg ("cmp",ix',ix) $ ix' == ix) xs of
     Nothing -> dbg (fmToList xst,"Nothing") xst
     Just (_,(_,True)) -> dbg "Just" xst
     Just (ps,_) -> let f (ix,_) _ = (ix,True)
                    in dbg ps $ addListToFM_C f xst (zip (inits ps) (repeat undefined))

find_struct x y = do
    (vl1, vl2) <- get_pick_line x y
    let r = vl2 `sub` vl1
    ps <- gui_var pick_geoms
    me <- gui_var cur_engine
    let ps' = sort $ filter (\(d,i) -> d < 0.1)
                   $ map (\(i, (s,_)) -> (absval $ dist vl1 r s,i)) ps
--    printf ("find: "`pv_vec2`" "`pv_vec2`" -> "&"\n") vl1 r ps'
    case (me, ps') of
     (Just eng, ((_,ix):_)) -> do
        xst <- gui_var strtree_state
        let xst' = expand_path xst ix
        printf ("strtree:  "&"  ->  "&"\n") (fmToList xst) (fmToList xst')
        Just ss <- gui_var cur_engine
        gui_upd (\gs -> gs { strtree_state = xst' })
        draw_struct (struct_tree ?gui)  ss
        let ps = se_find_struct eng ix
        printf ("\t\t"&"\n") ps
        old_es <- gui_var selected_node
        ifM old_es (\(_,e) -> do
            mapM_ (rm_entry (?gui.!selected_node_properties)) e)
        es <- add_prop_entries (?gui.!selected_node_properties) ps
        modifyIORef ?gui_ref (\gs -> gs { selected_node = Just (ix, es) })
        labelSetText (?gui.!selected_node_id) (show ix)
     _ -> return ()
    return ()

rm_entry c (_,_,EntryBox w _ _) = do
    containerRemove c w

mk_row b (s,p) = do
    h <- hBoxNew False 5
    l <- labelNew s
    e <- entryNew
    boxPackStart h l False False 5
    boxPackStart h e True True 5
    entrySetText e (show p)
    boxPackStart b h False False 5
    widgetShowAll b
    return (s,p,EntryBox h l e)

add_prop_entries box ps = do
    mapM (mk_row box) ps

parse_row (s,p,EntryBox h l e) = do
    p' <- read $^ entryGetText e
    return (s,p')

parse_prop_entries es = do
    ps <- mapM parse_row es
    return ps

update_node :: (?gui :: Gui, ?gui_ref :: IORef GuiState) => IO ()
update_node = do
    me <- gui_var cur_engine
    n <- selected_node $^ readIORef ?gui_ref
    case (me, n) of
     (Just eng, Just (ix,es)) -> do
        ps <- parse_prop_entries es
        let eng' = se_update_struct eng ix ps
        modifyIORef ?gui_ref (\gs -> gs { cur_engine = Just eng',
                                          geom_rep = Nothing } )
        update True ()
     _ -> return ()


inform_error hdr msg = do
    let wnd = inform_popup ?gui

    labelSetText (inform_popup_heading ?gui) hdr

    let Just black = colorParse "black"
    let Just white = colorParse "white"
    Just fixed <- fontLoad "fixed"
    textInsert (inform_popup_message ?gui) fixed black white msg
    widgetShowAll wnd   

conf_file = do
    home <- getEnv "HOME"
    return (home ++ "/.plantlabrc")

save_settings :: (?gui :: Gui, ?gui_ref :: IORef GuiState) => IO ()
save_settings = do
    dc <- gui_var gui_conf
    fn <- conf_file
    writeFile fn (unlines [show dc])


start_editor fn = do
    system ("gvim -rv " ++ fn)

hide_wnd w = do
    widgetHideAll w
    widgetUnrealize w

show_wnd w = do
    widgetShowAll w
    widgetRealize w

upd_dc f = modifyIORef ?gui_ref (\gs -> gs { gui_conf = f (gs.!gui_conf) } )

mod_of_fn = basename . remove_ext

create_template fn = do
    let mod = mod_of_fn fn :: String
    writeFile fn (
        "module " ++ mod ++ " where\n"++
        "\n"++
        "import Monad\n"++
        "import Seed2\n"++
        "import Types\n"++
        "import LinAlg\n"++
        "import MonadLib\n"++
        "import Misc\n"++
        "import EngineInst2\n"++
        "import SimEngine\n")
    readFile fn

main :: IO ()
main = do
    pn <- getProgName
    args <- getArgs
    (pn,args) <- Gtk.init (Just (pn, args))
--    geom  filter
    let argtable = pairify args
    Glade.init

    supported <- query
    unless supported $ do
      putStr "OpenGL not supported.\n"
      exit (ExitFailure 1)

    xml <- Glade.xmlNew "gui/gui.glade" Nothing -- (Just "window_main")
    gui <- GuiParts.gui xml

    windowSetTransientFor (inform_popup gui) (window_main gui)
--    show_wnd (inform_popup gui)
    let ip_wnd = inform_popup gui
    hide_wnd ip_wnd

    signalConnect (inform_popup_close gui)
        $ ButtonClickedHandler $ \_ -> hide_wnd ip_wnd

    ifM (break (=='+') $^ lookup "-geometry" argtable)
     (\(sz,pos) -> do
        ifM (read_pair 'x' sz) (\(w,h) ->
                widgetSetUSize (window_main gui) w h)
        ifM (read_pair '+' (drop 1 pos)) (\(x,y) ->
                widgetSetUPosition (window_main gui) x y))


    glarea <- areaNew [GdkGL.Rgba, GdkGL.Doublebuffer, GdkGL.DepthSize, toEnum 8]

    let rnd = mkStdGen 42
    let disp dc = gl_draw glarea () >> return ()
    let ipos = (0,0,3)
    let iori = ((0.3, -0.3, -0.1), 0.9)

    let gc = GuiConf False False False False True ipos iori (0,0,0) Nothing Nothing
    rot_ref <- newIORef (False, 0::Int, 0::Float, 0::Float)

    let ?gui = gui in do

    gui_ref <- newIORef $ GuiState [] Nothing Nothing Nothing [] gc emptyFM Nothing [] [] disp

    let ?gui_ref = gui_ref in do

    widgetSetEvents glarea [ExposureMask, ButtonPressMask, ButtonReleaseMask, PointerMotionMask]

    signalConnect glarea (WidgetExposeEventHandler gl_draw)
    signalConnect glarea (WidgetConfigureEventHandler gl_reshape)
    signalConnect glarea (WidgetRealizeHandler gl_init)

    let handle_btn ev fn = do
        btn <- eventButtonGetButton ev
        glview_transform rot_ref (fn btn)
        return True

    let update_cam_info gui = do
        dc <- gui_conf $^ readIORef ?gui_ref
        dc_to_gui dc

    update_cam_info gui
    signalConnect glarea $ WidgetMotionEventHandler
        (\_ ev -> do (x,y) <- get_btn_pos (eventMotionGetEventButton ev)
                     b <- glview_transform rot_ref (Motion x y)
                     when b $ do
                        update_cam_info gui
                        update False ()
                     return True)
    signalConnect glarea $ WidgetButtonPressEventHandler
        (\_ ev -> do (x,y) <- get_btn_pos ev
                     find_struct x y
                     handle_btn ev (Press x y))
    signalConnect glarea $ WidgetButtonReleaseEventHandler
        (\_ ev -> handle_btn ev Release)

    let mainwindow = window_main gui

    signalConnect mainwindow $ WidgetDeleteEventHandler $ \_ _ -> mainQuit >> return False
    quitAddDestroy 1 mainwindow

    boxPackStartDefaults (glarea_box gui) glarea

    age_adj <- rangeGetAdjustment (range $ age_slider gui)

    let statusbar = GuiParts.statusbar gui
    sb_id <- statusbarGetContextId statusbar "info"
    sb_ref <- newIORef (sb_id,[]::[Int])
    let sb_print msg = do 
                        (sb_id,is) <- readIORef sb_ref
                        mapM_ (statusbarRemove statusbar sb_id) is
                        id <- statusbarPush statusbar sb_id msg
                        writeIORef sb_ref (sb_id,[id])
                        timeoutAdd 5000 (TimeoutHandler $ do (sb_id,is) <- readIORef sb_ref
                                                             when (id `elem` is) $ statusbarRemove statusbar sb_id id
                                                             return False) >> return ()

    
    let sbprintf f = fmt (sb_print.concat) f []
    
    sb_print "startup"

    let simul_n n = do
        cage <- get_age
        sbprintf ("simulate to: "&"") (cage + n)
        dt <- profile_ $ mapM_ (const $ simulate) [1..n]
        age <- get_age
--        sbprintf ("Simulating at age: "&"") age
--        labelSetText (gui.!profile_time_sim) (show (dt))
        adjustmentSetValue age_adj $ fromIntegral age
        return True

    let simul_to age = do
            let tgt = round age
            current_age <- get_age
            case compare current_age tgt of
             EQ -> return ()
             LT -> simul_n (tgt-current_age) >> update True ()
             GT -> do gs <- readIORef ?gui_ref
                      let init_sims = gs.!sim_engines
                      ms <- gui_var (cur_engine_name.gui_conf)
                      ifM_ ms $ \s -> do
                        sb_print "simulation restarted"
                        case lookup s init_sims of
                         Nothing -> return ()
                         Just ss -> do writeIORef ?gui_ref (gs { cur_engine = Just ss,
                                                                 geom_rep = Nothing })
                                       simul_n tgt
                                       update True ()

    signalConnect age_adj $ AdjustmentValueChangedHandler $
        \_ -> do age <- adjustmentGetValue age_adj
                 simul_to age

    pm_adj <- rangeGetAdjustment (range $ interpolate_slider ?gui)
    signalConnect pm_adj $ AdjustmentValueChangedHandler $
        \_ -> do modifyIORef ?gui_ref (\gs -> gs { geom_rep = Nothing })
                 reset_geometry
                 update True ()

    wd_adj <- rangeGetAdjustment (range $ wind_slider ?gui)
    signalConnect wd_adj $ AdjustmentValueChangedHandler $
        \_ -> do modifyIORef ?gui_ref (\gs -> gs { geom_rep = Nothing })
                 eng <- gui_var cur_engine
                 wd <- adjustmentGetValue wd_adj
                 let f eng = se_set_env eng id
                 modifyIORef ?gui_ref (\gs -> gs { cur_engine = fmap f eng })
                 update True ()

    let simul_step d = do
         age <- get_age
         txt <- entryGetText (sim_step_interval gui)
         simul_to (fromIntegral age + d * read txt)
         update True ()

    signalConnect (sim_step gui) (ButtonClickedHandler (\_ -> simul_step 1))
    signalConnect (sim_step_back gui) (ButtonClickedHandler (\_ -> simul_step (-1)))

    connect_toggle (sim_run gui) $ \st -> do
        when st $ do
            timeoutAdd 100 $
                TimeoutHandler $ do
                    txt <- entryGetText (sim_step_interval gui)
                    simul_n (read txt)
                    st <- toggleButtonQueryState (sim_run gui)
                    update (not st) st
            return ()

    let auto_sim = do
            [tgt,step] <- mapM (liftM read . entryGetText) [autosim_target gui,
                                                            autosim_step gui]
            timeoutAdd 100
                (TimeoutHandler (do now <- get_age
                                    if now >= tgt then return False
                                     else do simul_n step
                                             reset_geometry
                                             update (now >= tgt-step) True))
            return ()

    signalConnect (autosim_run gui) $ ButtonClickedHandler $ \_ -> auto_sim

    signalConnect (gui.!selected_node_update) $ ButtonClickedHandler $ const update_node


-- Restore gui-state

    let force_regen = modifyIORef ?gui_ref (\gs -> gs { geom_rep = Nothing } )

    mapM_ (\(tg, f) -> do st <- toggleButtonQueryState tg 
                          upd_dc (f st)
                          connect_toggle_upd tg (\st -> upd_dc (f st)))
        [(toggle_draw_box    gui, \st r -> r { disp_box = st } ),
         (toggle_draw_ground gui, \st r -> r { disp_ground = st }),
         (toggle_mark_targets gui, \st r -> r { disp_target = st }),
         (toggle_draw_coords gui, \st r -> r { disp_coords = st })]

    tex_st <- toggleButtonQueryState (toggle_texturing gui)

    upd_dc (\r -> r { texturing = tex_st })

    connect_toggle_upd (toggle_texturing gui) $
        \st -> do upd_dc (\dc -> dc { texturing = st }); force_regen

    fn <- conf_file
    try (readFile fn >>= readIO >>= upd_dc . const)

    dc <- gui_var gui_conf
    dc_to_gui dc


    let Just black = colorParse "black"
    let Just white = colorParse "white"
    Just fixed <- fontLoad "fixed"


    widgetShowAll mainwindow

    let load = do
        fn <- gui_var (cur_model_file.gui_conf)
        ifM_ fn $ \fn -> do
            txt <- lines $^ readFile fn `catch` (const (create_template fn))
            let hdr = head txt
            let prog = unlines $ tail $ txt
            editableDeleteText (plantprog_text gui) 0 (-1)
            textInsert (plantprog_text gui) fixed black white prog
            let fn' | length fn > 15 = reverse . (++"...") . take 12 . reverse $ fn
                    | otherwise = fn
            labelSetText (plantprog_filename gui) fn'
            modifyIORef ?gui_ref (\gs -> gs { model_file_hdr = Just hdr })


    load `catch` (\_ -> sb_print "No such file!")

    

    dyn_ref <- newIORef (0::Int)
    changed_ref <- newIORef False

    let write_mod = readIORef changed_ref >>= \b -> when b $ do
        
        fn <- gui_var (cur_model_file.gui_conf)
        maybeM_ (\fn -> do
            let mod = mod_of_fn fn
            hdr <- (maybe ("module "++mod) id) $^ gui_var model_file_hdr
            txt <- textGetText (plantprog_text gui)
            let lns = takeWhile (\l -> not ("mk_plants" `isPrefixOf` l)) $ lines txt
            let ps = nub $ map (head . words) $ filter ("plant_" `isPrefixOf`) lns
            let mk_plants = ["",
                             "mk_plants :: IO [(String, SimEngine)]",
                             "mk_plants = sequence ("] ++
                             map (\p -> "        liftM ((,) \"" ++ drop 6 p ++
                                            "\" ) (mk_sim_engine (seed " ++ p ++ ")) : ") ps ++
                                    ["    [])","",""]
            writeFile fn (unlines (hdr:lns ++ mk_plants))
            writeIORef changed_ref False
          ) fn

    let resimul m_age = do
        let age = maybe 0 id m_age
        model <- entryGetText (model_entry gui)
        l <- gui_var (cur_engine_name.gui_conf)
        (sims,sim) <- (\gs -> (gs.!sim_engines, gs.!cur_engine)) $^ readIORef ?gui_ref
        let pair (Just (a,b)) = (Just a, Just b)
        let Just (l',sim') =
              (do s <- lookup model sims; return (model, Just s)) `mplus`
              (do ls <- l; s <- lookup ls sims; return (ls, Just s)) `mplus`
              (do (a,b) <- listToMaybe sims; return (a, Just b)) `mplus`
              Just ("<none>", Nothing)
        force_regen
        modifyIORef ?gui_ref (\gs -> gs { cur_engine = sim' })
        upd_dc $ \dc -> dc { cur_engine_name = Just l' }
        comboSetPopdownStrings (model_combo gui) (map fst sims)
        entrySetText (model_entry gui) l'
        sbprintf "reload done"
        simul_n age
        update True ()
        return ()

    let all_mods =
            ["Seed2", "Simul2", "Draw2", "FuncExpr", "IdentMonad", "EngineInst2"] ++
            ["NonBasic", "STArrLib", "BArrAtoms", "Helpers", 
             "SimStateHelpers", "SimStateIHelpers", "Draw", 
             "MetaLib", "Meta", "MetaPlants", "MonInter", 
             "Run", "EngineInst1", "Wither", "MetaLibHigh", "MetaLibLow"]

    let reload = do
        age <- get_age

        editableDeleteText (inform_popup_message ?gui) 0 (-1)
        sbprintf "reloading plant"
        modifyIORef dyn_ref succ
        fn <- gui_var (cur_model_file.gui_conf)
        maybeM_ (\fn -> do
            let mod = basename $ remove_ext fn
            newmod <- ((mod++).show) $^ readIORef dyn_ref

            done_fn <- LM.compile mod newmod
            sbprintf ("compiling "&."") mod

            -- Wait for compilation to finish, then load
            timeoutAdd 100 $ TimeoutHandler $
             do r <- done_fn
                case r of
                 Nothing -> return True
                 Just (Left (out,err)) -> inform_error "Compilation errors" (out++err) >> return False
                 Just (Right (ok,_)) -> do 
                            sbprintf ("compilation of "&." finished, restarting") mod
                            dummy_simul
                            simul_to 0

                            Just h <- LM.load_obj newmod
                            mk_sim <- LM.load_sym h (newmod ++ "_mkzuplants_closure")
                            sims <- mk_sim
                            modifyIORef ?gui_ref (\gs -> gs { sim_engines = sims,
                                                              cur_engine = Nothing,
                                                              geom_rep = Nothing })
                            resimul (Just age)
                            sbprintf "done"
                            return False
            return ()) fn

    mapM_ (\(w, m) -> signalConnect (w gui) (ButtonClickedHandler (const m))) $
        (plantprog_go, write_mod >> reload) :
        (model_change, resimul Nothing) :
        (plantprog_edit, gui_var (cur_model_file.gui_conf) >>= (`ifM_` start_editor)) :
        []

--    timeoutAdd 1000 $ TimeoutHandler $ (printf "tick\n" >> return True)

    -- Compile and load simulation modules in the background..
    iax <- LM.init
    iar <- newIORef iax

    let spawn_job = do
            iax <- readIORef iar
            case iax of
                [] -> mapM_ LM.load_obj all_mods >> reload >> return False
                (i:ax) -> do writeIORef iar ax
                             (info,r) <- i
                             sbprintf info
                             case r of
                               Just test -> timeoutAdd 10 (TimeoutHandler (wait_ac test)) >> return False
                               Nothing -> spawn_job

        wait_ac ac = do
            r <- ac
            case r of
             Nothing -> return True
             Just (Left (out,err)) -> inform_error "Compilation errors" (out++err) >> return False
             Just (Right (ok,_)) -> do
                timeoutAdd 10 (TimeoutHandler spawn_job)
                return False
            
    spawn_job

    let bind_menu name action =
            gui_connect xml name
                (Gtk.MenuItemActivateHandler ((const action) :: MenuItem -> IO ()))
    mapM_ (uncurry bind_menu) $
         ("menu_exit", mainQuit) :
         ("menu_open", open_dialog (load >> write_mod >> reload)) :
         ("menu_reload", write_mod >> reload) :
         ("menu_export_pov", export_pov) :
         ("menu_export_flat_geomrep", export_flat_geomrep) :
         ("menu_export_geomrep", export_geomrep) :
         ("menu_import_geomrep", import_geomrep) :
         ("menu_sim_step", buttonClicked (sim_step gui)) :
         ("menu_sim_step_back", buttonClicked (sim_step_back gui)) :
         ("menu_sim_run", do st <- toggleButtonQueryState (sim_run gui)
                             toggleButtonSetActive (sim_run gui) (not st)) :
         ("menu_settings_save", save_settings) :
         ("menu_sim_auto", auto_sim) :
         []
    
    Glade.xmlSignalConnect xml "plantprog_text_changed" $
        EditableChangedHandler ((const (modifyIORef changed_ref (const True))) :: Editable -> IO ())

    Gtk.main
    exitWith ExitSuccess



