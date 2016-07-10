module Macro where

data Macro = Evolve Int
           | PovEx String
           | SetRes Float
           | Clone
           | DryRen

macra = macra_1

macra_0 = [(Evolve 5), DryRen, (PovEx "ev0.3d"), 
         (Evolve 5), DryRen, (PovEx "ev1.3d"),           
         (Evolve 5), DryRen, (PovEx "ev2.3d"), 
         (Evolve 5), DryRen, (PovEx "ev3.3d"),           
         (Evolve 5), DryRen, (PovEx "ev4.3d"),
         (Evolve 5), DryRen, (PovEx "ev5.3d"), 
         (Evolve 5), DryRen, (PovEx "ev6.3d"),           
         (Evolve 5), DryRen, (PovEx "ev7.3d"),
         (Evolve 5), DryRen, (PovEx "ev8.3d")]

macra_1 = [(Evolve 50), 
           DryRen, (PovEx "nev0.3d"),
           Clone, DryRen, (PovEx "nev1.3d"),
           Clone, DryRen, (PovEx "nev2.3d")]

macra_2 = [(SetRes 0.01), (Evolve 5), DryRen, (PovEx "ev0.3d"),
           (SetRes 0.1), (Evolve 40), DryRen, (PovEx "ev1.3d")]

          
