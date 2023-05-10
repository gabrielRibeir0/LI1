{-|
Module : Aula 4
Description : Módulo Haskell com uma função Haskell
Copyright : (c) Gabriel RIbeiro <a104171@alunos.uminho.pt>;
Gabriel Ribeiro <a104171@alunos.uminho.pt>
-}
module Aula4 where

{-|
A função 'max2' devolve o maior de 2 números inteiros (@max2 m n@ verifica qual dos números @__m__@ e @/__n__/@ é maior e devolve-o)

A __função__ pode ser definida da seguinte /forma/:

@
max2 a b = if a > b then a else b
@

Também pode ser definida usando guardas:

@
max2 a b = | a > b = a
           | otherwise = b
@

== Exemplos de utilização

>>>max2 4 2
4

>>>max2 5 3
5

>>>max2 5 18
18

=== Propriedades
prop>Esta é uma função não recursiva

* É uma função que pode ser definida mais do que uma forma usando __guardas__ ou @/if ... then ... else/@

* Esta função recebe dois inteiros (@Int@) e devolve um inteiro (@Int@)
-}
max2 :: Int -> Int -> Int
max2 a b = if a > b then a else b