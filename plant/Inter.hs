module Inter where

import NonBasic
import LinAlg
import Types
import PlantTypes
import Helpers
import Sun
import Misc

import List
import Random

-- interpretator 0.01a
-- Simula!   

p0::Point 
p0 = (0.0, 0.0, 0.0)

simula::Rnd -> Seed -> Environment -> (Power,Time) -> BranchState -> (Environment, BranchState)
simula r s e (p,t) bs = dbg dg $
                                case (psim2 s e 0.0 r (origo, id_quat) (root_p,t) (Just bs)) of
                                             (nenv, _,(Just bs_sim)) -> (nenv, bs_sim)
                                             (nenv, _,Nothing) -> (nenv, null_state)
            where g s bs = (show  (findnode (bs_pin bs) s))  ++ ", " ++ (concatMap (g s) (bs_bs bs))
                  root_p = let (BS _ _ _ rad _ _ _ _ _ _ _) = bs
                               (Se earth _) = s
                           in  ( (ea_rootf earth) rad p)
                  leaves = length (en_sunw e)                           
                  dg = (show root_p) ++ " / " ++ (show (bs_dev bs) )
                  -- dg2 = (show (length (en_sunw e))) ++ " / " ++ (show (foldr max 0.0 (map (\((x,y,z),r) -> y) (en_sunw e)) ) )
                  dg2 = map (\((x,y,z),r) -> y) (en_sunw e)

kill_leaves::Seed -> Time -> BranchState -> BranchState
kill_leaves seed t this@(BS bs dir pos rad dev age nage pin min ali more) = 
    case findnode pin seed of
        Nothing -> (BS bs dir pos rad dev (age + t) (nage + t) pin min ali more)
        Just (Le _ _ _ _) -> (BS bs dir pos rad dev (age + t) (nage + t) pin min False more)
        Just (Br _ _) -> let bs' = map (kill_leaves seed t) bs
                       in (BS bs' dir pos rad dev (age + t) (nage + t) pin min ali more)

psim2::Seed -> Environment -> Development -> Rnd -> (Point, Quat) -> (Power,Time) -> Maybe BranchState -> (Environment, Power, Maybe BranchState)
psim2 seed env ma r (mspos,q) (p,t) (Just this@(BS bs dir pos rad dev age nage pin min ali more)) = 
        let (r1:r2:r3:r4:r5) = rnd_gens r

            (env',pow',bs') = feedleaves r1 env p pos_and_q bs
            (env'', pow'', bs'') = feedbranches r2 env' pow' bs'
            
            (my_pow, ret_emod) = case pow'' of
                                   (Left x) -> (x,0.0)
                                   (Right x) -> if (min /= []) then (0.0,x) else (x,0.0)
                                              
            sim_me_t = sim r3 seed env'' ma (mspos, q) (my_pow, t) (Just (this {bs_bs = bs'', bs_rad = new_rad}))
            (ret_ovf, nenv, Just sim_me) = sim_me_t 
            new_rad = max ( (sqrt (lc this)) / 7.0 ) rad          
            me = sim_me 

            kill_me = kill_leaves seed t this
            
            dg =  (show my_pow) ++ " | " ++ (show pin) ++ " | power me "
            dg5 = (show pow'') ++ " | " ++ (show pow') ++ " | " ++ (show pin) ++ " | power overflow"
            dg2 = (show my_dist) ++ " <- my_dist "
            dg3 = (show p) ++ " | " ++ (show pin) ++ " | power in "
            dg4 = (show pin) ++ " <- kill "
            dg6 = (show my_pow) ++ " <- mypow | " ++ (show min) ++ " <- min | " ++ (show pin) ++ " <- pin | " ++ (show my_dist) ++ " <- my_dist | " ++ (show p) ++ " <- power in | " ++ (show dev) ++ " <- dev | "
            dg7 = (show pow'') ++ " <- pow'' | " ++ (show pow') ++ " <- pow' | " ++ (show p) ++ " <- p | "
            
        in if p <= 0.0 then (env'', 0.0, Just kill_me)
                       else (env'', ret_ovf + ret_emod, Just me)

        where feedleaves r env pow _ [] = if pow < 0.0 then error "mjo" else (env, pow, [])
              feedleaves r env pow (q:qs) (bs:bss) = 
                let (env', pow',bss') = feedleaves r1 env pow qs bss
                    (r1,r2) = split r       
                    (pow'', env'' ,  bs'') = if (isLeaf (bs_pin bs) seed) 
                                                then let (lpow, lenv, lbs) = (sim r2 seed env' dev q ((le_cons le), t) (Just bs))
                                                         (Just le) = findnode (bs_pin bs) seed         
                                                     in if (le_cons le) < pow' then (pow' - (le_cons le), lenv, lbs)
                                                                               else (pow', env', Just bs)
                                                else (pow', env', Just bs)
                    
                                                                          
                    nbss = case bs'' of
                            Nothing -> bss'
                            (Just nbs) -> (nbs:bss')
        
                    dg = (show pow'') ++ "<- pow'' | " ++ (show pow') ++ " <- pow' | " ++ (show pow) ++ " <- pow to leaves "
                     
                    ret = (env'', pow'', nbss)
                            
                in ret -- dbg dg $ ret
             
              feedbranches::Rnd->Environment->Power->[BranchState]->(Environment, Either Float Float, [BranchState])
              feedbranches r env pow []  = (env, Left pow, [])                                                                 --- sim r2 seed env   ma (mspos, q) (my_pow, t) (Just me)                               
              feedbranches r env pow bss = 
                let is_leaf = (\bs -> (isLeaf (bs_pin bs) seed))
                                    
                    bdl1 = bdl bss
                    brfnc (bs,dv) = if (is_leaf bs) then 0.0 else dv -- dbg ("dv: " ++ (show dv) ++ " my_dist: " ++ (show my_dist)) $ dv
                    bdl2 = map brfnc (zip bss bdl1) 
                    bdlsum = case my_dist of
                                    Nothing -> foldr (+) 0.0 bdl2
                                    Just x  -> foldr (+) x bdl2
                    bdl3 = map (\x -> x / bdlsum) bdl2
                    pws = map2 (*) bdl3 (repeat pow)
                    pqs = pos_and_q
                    dg = "power " ++ (show (filter (/= 0) pws))
                    just_leaves = (length (filter (== 0) pws)) == 0
                    ret = if just_leaves then (env, sendback pow, bss)
                                         else let (env', pow', bss') = feedbranches2 env t (rnd_gens r) pws pqs bss
                                         
                                                  md = case my_dist of
                                                         Just x -> x
                                                         Nothing -> 0.0
                                                         
                                                  dist_pow_mot = (pow*md/bdlsum)
                                                  ret_mot = pow' + dist_pow_mot
                                                  ps = map (\bs -> (bs_pin bs)) bss
                                                  dg1 = show $ zip ps bdl2
                                                  dg = ((show (ret_mot:pow':pow:bdlsum:dist_pow_mot:pws)) ++ " <- pws " ++ (show bdlsum) ++ " <- bdlsum | " ) 
                                                  iret = if ret_mot < 0.0 then dbg dg $ error "no means no, you bastard" -- (env', ret_mot, bss')
                                                                          else (env', sendback ret_mot, bss')
                                              in (env', sendback ret_mot, bss') 
                in ret                              
            
              sendback x = case my_dist of
                            Nothing -> Right x        
                            Just _  -> Left x
                                
              feedbranches2::Environment -> Time -> [Rnd] -> [Power] -> [(Point,Quat)] -> [BranchState] -> (Environment, Power, [BranchState])
              feedbranches2 env time _ _ _ [] = (env, 0.0, [])                    
--               feedbranches2 env time (r:rs) (0.0:ps) (q:qs) (bs:bss) = feedbranches2 env time rs ps qs bss
              feedbranches2 env time (r:rs) (p:ps) (q:qs) (bs:bss) = 
                let (env',pow',bss') = feedbranches2 env time rs ps qs bss
                    (r1,r2) = split r
                    (env'',pow'', Just bs'') = psim2 seed env' dev r1 q (p,t) (Just bs)
                in if p /= 0.0 then (env'', pow'', (bs'':bss'))
                               else (env', pow', bs:bss')
                               
                                                            
              my_dist = let param = if min == [] then RootSelf else Self -- if ma == 0 then 1.0 else 1.0 -- (pos / ma)
                            mdd = ma - pos
                            ret = (get_disfunc seed pin) param 1 dev mdd
                            dg = "ma: " ++ (show ma) ++ " pos: " ++ (show pos) ++ " dist: " ++ (show ret) ++ " | " ++ (show p) ++ " <- power in | " 
                            Just x = ret --  dbg dg $ ret
                        in ret
                
              bdl bs = let dfnc = get_disfunc seed (pin ++ [0]) 
                           arl = map (\bs-> Children ((bs_pos bs)/dev)) bs -- if age == 0 then (pos/dev) else (pos/dev)) bs
                           lcl = map leaf_count bs
                           del = map (\bs -> (bs_dev bs)) bs
                           mdl = map (\bs -> dev - (bs_pos bs)) bs
                           reti = map4 dfnc arl lcl del mdl
                       in map (\(Just x) -> x) reti
             
              nq = mulq q (rot_q_y dir)                                                                                 
              pos_and_q = zip (map (\bs -> getMSPos pin seed (bs_pos bs) mspos nq) bs) (repeat nq)
              
              
              lc bs = let v = if (isLeaf (bs_pin bs) seed) then 1
                                                           else 0
                      in foldr (+) v (map lc (bs_bs bs))
                                                                                        

psim::Seed -> Environment -> Development -> Rnd -> (Point, Quat) -> (Power,Time) -> Maybe BranchState -> (Environment, Power, Maybe BranchState)
psim seed env ma r (mspos,q) (p,t) (Just this@(BS bs dir pos rad dev age nage pin min ali more)) = 
    let 
        lclsum = foldr (+) 0.0 lcl                                                      -- leaf_count_sum
        new_rad = max ( (sqrt lclsum) / 10.0 ) rad
        ovf = p - (lclsum * leaf_cons)                                                  -- over_flow
        bdlsum = (foldr (+) my_dist bdl) 
        ovfl = map (\x-> x*(ovf/bdlsum)) bdl
        (r1,r2) = split r
        eplist = let ret = hmf seed env dev (rnd_gens r1) pos_and_q (zip ovfl tl) bs    -- pair_list
                     (_,ptmp,_) = ret
                 in ret -- dbg ("ptmp: " ++ (show ptmp) ++ " endptmp") $ ret
        (cenv, retovf, sim_bs) = eplist
        my_pow = if bdlsum == 0.0 then ovf + retovf
                                  else (my_dist * (ovf/bdlsum)) + retovf                                      -- my_power
        me = (BS sim_bs dir pos new_rad dev age nage pin min ali more)
        sim_me_t = sim r2 seed cenv ma (mspos, q) (my_pow, t) (Just me)
        (ret_ovf, nenv, sim_me) = sim_me_t
        -- nenv = fst sim_me_t
        kill_me = kill_leaves seed t this
        -- e0 =  map (\((x,y,z),r) -> y) (en_sunw cenv)
    in if ovf < 0.0 then (nenv, 0.0, Just kill_me)
                    else  dbg ((show ret_ovf) ++ " <- ret_ovf") $ (nenv, ret_ovf, sim_me)
                                                          
        where nq = mulq q (rot_q_y dir)
                           
              hmf::Seed -> Environment -> Development -> [Rnd] -> [(Point, Quat)] -> [(Power,Time)] -> [BranchState] -> (Environment, Power, [BranchState])
              
              hmf seed env dev _ _ _ [] = dbg ("hmf1:") $ (env, 0.0, [])
              
              hmf seed env dev (r:rs) (p:ps) (o:os) (b:bs) =              
                    let (env', p', bs') = hmf seed env dev rs ps os bs
                        (env'', p'', bs'') = psim seed env' dev r p o (Just b)
                    in dbg ( (show (p'')) ++ " " ++ (show (fst o))  ++ " <- hmf:") $ (env'', p' + p'', ( (f bs'') ++ bs'))
              
              f bs = case bs of
                        Nothing -> []
                        (Just b)-> [b]
              
              pos_and_q  = zip (map (\bs -> getMSPos pin seed (bs_pos bs) mspos nq) bs) (repeat nq)
        
              my_dist = let param = Self -- if ma == 0 then 1.0 else 1.0 -- (pos / ma)
                            mdd = ma - pos
                            ret = (get_disfunc seed pin) param 1 dev mdd
                            dg = "ma: " ++ (show ma) ++ " pos: " ++ (show pos) ++ " dist: " ++ (show ret)
                            Just x = ret
                        in dbg dg $ x
              tl = t:tl
              
              lcl = case (findnode pin seed) of 
                        Just (Le _ food _ _) -> food:child_leaves
                        _ -> child_leaves
                    where child_leaves = map leaf_count bs
                    
              
--                         (BS [] _ _ _ _ _ _ _ _ _) = 1
--              leaf_count (BS bs _ _ _ _ _ _ _ _ _) = foldr (+) 0 (map leaf_count bs)                                                          
              leaf_cons = 0.01
              bdl = let dfnc = get_disfunc seed (pin ++ [0]) 
                        -- dfl = map (\bs -> get_disfunc seed (bs_pin bs)) bs
                        arl = map (\bs-> Children ( (bs_pos bs)/dev ) ) bs -- if age == 0 then (pos/dev) else (pos/dev)) bs
                        lcl = map leaf_count bs
                        del = map (\bs -> (bs_dev bs)) bs
                        mdl = map (\bs -> dev - (bs_pos bs)) bs
                        reti = map4 dfnc arl lcl del mdl
                        ret = map (\(Just x) -> x) reti
                    in ret

leaf_count bs = case (bs_bs bs) of
                [] -> 1
                _  -> foldr (+) 0 (map leaf_count (bs_bs bs))

 
sim::Rnd -> Seed -> Environment -> Development -> (Point, Quat) -> (Power,Time) -> Maybe BranchState -> (Power, Environment, Maybe BranchState)
sim _ _ e _ _ (p,q) Nothing =  (p,e,Nothing)
sim r s env ma (mspos,q) (pow, time) (Just bs) = 
    let g = grow env (se_earth s);  dd = time*pow*g  -- delta_development

        (r1:r2:r3:r4:r5:rs) = rnd_gens r

        pin = (bs_pin bs)

        is_leaf = case findnode pin s of
                         (Just leaf@(Le _ _ _ _)) -> True
                         _ -> False
                                                                        
                                                  
        is_endleaf = ((List.length (bs_pin bs)) - (List.length (bs_min bs))) == 1                         
                         
        is_dead = not (bs_alive bs)                         
                
        lev_limit = let (Cr inf _ _) = findcrown ( (bs_min bs) ++ [0]) s
                    in (if_sdev inf)
                            
        leaf_limit = let (Cr inf _ _) = findcrown (findleaf (bs_pin bs) s) s
                     in (if_sdev inf)
               
        d = lev_limit * (snd $ properFraction $ ( (bs_pos bs) / (lev_limit) ) )

        -- Should current leaf evolve into its corresponding branch-level branch?
        {-
        levev = let inf = findif ((bs_min bs) ++ [0]) s
                    evfnc = (if_evfnc inf)
                    (normran, r_tmp) = randomR (0,1) r5
                    res =  evfnc normran (bs_pos bs) ma
                in res -- dbg ( (show res) ++ " <- levev") $ res
        -}
        evolve = let ret = ( (bs_min bs) == []) || {- (d <= leaf_limit) && -} (bs_more bs)
                     dg = (show (bs_more bs)) ++ " <- bs_more | " ++ (show ( (bs_min bs) == [])) ++ " <- bs_min"
                 in ret -- dbg dg $ ret
    
        leaf_sur = let Just (Le _ _ sf _) = findnode (bs_pin bs) s
                   in sf ma (bs_pos bs)
        
        leafpos = mspos -- getMSPos pin s (bs_dev bs) mspos q
        leaf_in_shadow = False -- let r = not $ leafInSun leafpos env
                         -- in r -- dbg "shadow" $ dbg (bs_pos bs) $ dbg leafpos $ r
                   
        
        cur_dev = (bs_dev bs)
        d1 = leaf_limit * (1.0 - ( snd (properFraction (cur_dev / leaf_limit)) ) ) -- before addstruct
        d2 = dd - d1
        t2 = d2/(g*pow)
        t1 = d1/(g*pow)
        
     in if is_leaf then if leaf_in_shadow then dbg "dropped!" $ (pow, leafFromEnv leafpos env, Nothing)
                                          else if is_dead then if is_endleaf then c0 r1 r2
                                                                             else if (not evolve) then if leaf_sur then c1 r1 r2
                                                                                                                   else c2 
                                                                                                  else c3 r1 r2
                                                          else c4 r1
                   else if d1 < dd then c5 t1 t2 r1 r2 r3
                                   else c6 r1
                                      
     where  c0 r r' = let (nenv, nbs) = (addstruct r s env False ma (mspos, q) (Just bs)) 
                      in sim2 r' s nenv ma (pow, time) nbs
    
            c1 r r' = let (nenv, nbs) = (addstruct r' s env False ma (mspos, q) (Just bs))
                      in sim2 r s nenv ma (pow, time) nbs
                 
            c2 =  (pow, env, Nothing)
            
            c3 r r' = let (nenv, nbs) = (addstruct r' s env True ma (mspos,q) (Just bs))
                      in  sim r s nenv ma (mspos, q) (pow, time) nbs
                 
            c4 r =  sim2 r s env ma (pow,time) (Just bs)
            
            c5 t1 t2 r r' r''= let (p, nenv, nbs) = (sim2 r'' s env ma (pow,t1) (Just bs))
                                   (nenv', nbs') = addstruct r' s nenv True ma (mspos,q) nbs
                                   d0 = map (\((x,y,z),r) -> y) (en_sunw env) 
                                   d1 = map (\((x,y,z),r) -> y) (en_sunw nenv') 
                                   snbs = let (Just v) = nbs in v
                                   d3 = getMSPos (bs_pin bs) s (bs_dev bs) mspos q
                                   d4 = getMSPos (bs_pin bs) s (bs_dev snbs) mspos q
                                   ret = sim r s nenv' ma (mspos, q) (pow,t2) nbs'
                               in  ret

            c6 r = sim2 r s env ma (pow,time) (Just bs)              
            
grow::Environment -> Earth -> Float
grow _ ea = (ea_boost ea)

rnd_gens r = let (a,b) = split r in a:rnd_gens b

sim2::Rnd -> Seed -> Environment -> Age -> (Power,Time) -> Maybe BranchState -> (Power, Environment, Maybe BranchState)
sim2 rnd seed env ma (pow,time) (Just this@(BS bs dir pos rad dev age nage pin min ali more)) = 
          let dead = False -- age > ma / 2 && ma > 0.0
          in if dead then  (pow,env, Nothing) -- (BS bs' dir pos rad dev age nage pin min ali)
                     else let nbs =  (BS bs dir pos rad (dev + delta_dev) (age + time) (nage + time) pin min ali more)
                              p = case findnode pin seed of
                                       Just (Le _ food _ _) -> pow - food
                                       _                    -> 0.0                 
                              ret = (p, env, Just nbs)
                          in ret -- dbg ("p: " ++ (show p)) $ ret
     where delta_dev = let boost = grow env (se_earth seed)
                           ret = boost * time * pow
                             
                       in ret
          -- decide_addstruct newage seed = newage > 1.0
          
sure [] = []
sure (Just bs:bss) = bs:(sure bss)
sure (Nothing:bss) = sure bss          
                                           
addstruct::Rnd -> Seed -> Environment -> Bool -> Development -> (Point, Quat) -> Maybe BranchState -> (Environment, Maybe BranchState)
addstruct _ _ env _ _ _ Nothing = (env, Nothing)
addstruct r seed env leafdev ma (mspos, q) (Just child@(BS bs dir pos rad dev age nage pin min ali more)) =
                            case findnode pin seed of
                             Nothing -> let nlt = (newleaves env r 0.0 1)
                                            np = getMSPos pin seed dev mspos q
                                            nbs = (head (snd nlt))
                                            nbs' = nbs {bs_pos = 0.0}
                                        in (fst nlt, Just nbs')
                             Just (Le _ _ _ _) -> if (not ali) then if ( ((length pin) - (length min)   > 1) && leafdev )
                                                                     then let (r1:r2:rs) = (rnd_gens r)
                                                                          in (env, Just (BS bs (ndir r1) pos rad 0.0 0.0 0.0 newbranch_id min ali more))
                                                                     else (env, Just child)
                                                               else (env, Just child)
                             Just (Br _ _) -> let nlt = newleaves env r dev sprouts
                                                  np = getMSPos pin seed dev mspos q
                                                  nbs = (bs ++ (snd nlt)) 
                                              in  (fst nlt, Just (BS nbs dir pos rad dev age 0.0 pin min ali more))

        where fwd = rot_vec_q (0,1,0) q
              newbranch_id = (take ((length min) + 1) pin)
              ndir r = let (rnd, r') = randomR (0,1) r
                           c = findcrown newbranch_id seed 
                       in (dirfunc rnd fwd c)
              
              newleaves::Environment -> Rnd -> Development -> Spawncnt -> (Environment, [BranchState])
              newleaves env r pos 0 = (env, [])
              newleaves env r pos n = let (r1:r2:r3:rs) = rnd_gens r
                                          (rnd,r') = randomR (0,1) r1
                                          (renv, bs) = newleaves env r2 pos (n-1)
                                          dir = (dirfunc rnd fwd leaf)

                                          (normran, r_tmp) = randomR (0,1) r3
                                          
                                          levev = let inf = findif (pin ++ [0]) seed
                                                      evfnc = (if_evfnc inf)
                                                      res =  evfnc normran dev ma
                                                  in res
                                          
                                          lpos = getMSPos pin seed dev mspos q
                                          -- lpos = getMSPos lpin seed 0.0 mspos q
                                          nenv = leafToEnv lpos renv
                                          dg2 = (show levev) ++ " <- levev | " ++ (show normran) ++ "<- normran | "
                                          dg = "newleaf: " ++ (show lpos) ++ " " ++ (show dev)
                                      in (nenv, BS [] dir dev 0.0 0.0 0.0 0.0 lpin pin True levev:bs)
              -- sprouts = let (Se earth _) = seed in (ea_brate earth)
              sprouts = let inf = findif lpin seed
                        in (if_scnt inf)
              leaf = findcrown lpin seed
              lpin = findleaf pin seed

leafInSun:: Point -> Environment -> Bool
leafInSun p env = leafHitsSun p (en_sunw env)

leafFromEnv:: Point -> Environment -> Environment
leafFromEnv p env = let sphere = (p, 1.0)
                        sunw = (en_sunw env)
                        nsunw = sphereFromSun sphere sunw
                        nenv = env { en_sunw = nsunw }
                    in dbg p $ dbg (f env) $ dbg "leaving leaf: " $ dbg (length sunw) $ dbg (length nsunw) $ nenv
                    
                    where f e = map (\((x,y,z),r) -> y) (en_sunw e)    

leafToEnv:: Point -> Environment -> Environment
leafToEnv p env = let leafrad = 0.1
                      sphere = (p, leafrad)
                      sunw = (en_sunw env)
                      nsunw = sphereToSun sphere sunw
                      nenv = env {en_sunw = nsunw}
                  in nenv
                  
mergeenv::[Environment] -> Environment
mergeenv envs = let ret = foldr merge null_env envs
                    dg0 = map f envs
                    dg1 = f ret
                in ret
    
    where f e = map (\((x,y,z),r) -> y) (en_sunw e)
          merge e0 e1 = let s0 = en_sunw e0
                            s1 = en_sunw e1
                            ns = mergeSuns s0 s1
                            -- dg = (show (length s0)) ++ " + " ++ (show (length s1)) ++ " = " ++ (show (length ns))
                        in (e1 {en_sunw = ns})