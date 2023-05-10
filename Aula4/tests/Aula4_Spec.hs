module Aula4_Spec where
import Test.HUnit
import Aula4

testes_Aula4 = test ["Teste 1 (6, 4)" ~: 6 ~=? max2 6 4,
                     "Teste 2 (140, 72)" ~: 140 ~=? max2 140 72,
                     "Teste 3 (13, 13)" ~: 13 ~=? max2 13 13,
                     "Teste 4 (4, 12)" ~: 12 ~=? max2 4 12,
                     "Teste 5 (-5, -1)" ~: -1 ~=? max2 (-5) (-1)]