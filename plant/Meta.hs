
module Meta where

import Helpers
import NonBasic
import Types
import PlantTypes
import LinAlg
import Misc (dbg, mapf, mapf2)

------------------------------------
-- Meta Language ---------------------
--------------------------------------

br_setthick::Float -> PlantNode -> PlantNode
br_setthick v pn = pn {br_thick = v}

if_setmod :: ModDevFnc -> Interface -> Interface
if_setmod mdf inf = inf {if_moddev = mdf}

if_setdist :: DistFunc -> Interface -> Interface    
if_setdist df inf = inf {if_bdist = df}

if_setevol :: EvFnc -> Interface -> Interface
if_setevol ef inf = inf {if_evf = ef}

if_setdir :: Angledist -> Interface -> Interface
if_setdir ad inf = inf {if_adist = ad}

if_setspwn :: Spawncnt -> Interface -> Interface
if_setspwn n inf = inf {if_scnt = n}

infixl 3 <<.
(<<.)::(Interface, PlantNode) -> (Interface, PlantNode) -> Crown
(<<.) (i0,p0) (i1,p1) = (Cr i0 p0 [Cr i1 p1 []])

infixr 2 <<
(<<)::(Interface, PlantNode) -> Crown -> Crown
(<<) (i,p) (Cr ci cp cs) = (Cr i p [Cr ci cp cs])

infixl 1 .<<
(.<<)::(Earth, Common) -> Crown -> Seed
(.<<) (e,com) c = Se e com [c]

