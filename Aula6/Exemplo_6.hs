module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

data Jogo = Jogo Jogador [(Int, Int)]

type Jogador = (Int, Int)

data Opcao = Jogar
            | Sair

data Menu = Opcoes Opcao
          | ModoJogo 
          | VenceuJogo

type World = (Menu, Jogo, Images, Time)

type Time = Float

type Images = [Picture]

window :: Display
window = InWindow "Pacman" (700, 700) (0,0)

fr :: Int
fr = 50

initialState :: Images -> World
initialState images = (Opcoes Jogar, Jogo (0, (-300)) [(50, 50), (-250, -100), (-100, -50)], images, 0)

drawState :: World -> Picture
drawState (VenceuJogo, jogo, images, n) = Translate (-200) 0 $ Color red $ Text "Ganhou"
drawState (Opcoes Jogar, jogo, images,n) = Pictures [Color blue $ drawOption "Jogar", Translate 0 (-70) $ drawOption "Sair"]
drawState (Opcoes Sair, jogo, images,n) = Pictures [drawOption "Jogar", Color blue $ Translate 0 (-70) $ drawOption "Sair"]
drawState (ModoJogo, (Jogo (x, y) l), images,n) = Pictures $ (map drawSmallCircles l) ++ [Translate i j $ pacman]
  where
      i = fromIntegral x
      j = fromIntegral y
      pacman = if (mod (round (n*1000)) 200) < 100 then (head images) else (last images)

drawOption option = Translate (-50) 0 $ Scale (0.5) (0.5) $ Text option

drawSmallCircles :: (Int, Int) -> Picture
drawSmallCircles (x, y) = Translate i j $ Circle 5
    where
      i = fromIntegral x
      j = fromIntegral y

newState :: (Int, Int) -> [(Int, Int)] -> Jogo
newState p l = Jogo p (filter (p/=) l)

event :: Event -> World -> World
-- Menu
event (EventKey (SpecialKey KeyEnter) Down _ _) (Opcoes Jogar, jogo, i, n) =
   (ModoJogo, jogo, i, n)
event (EventKey (SpecialKey KeyUp) Down _ _) (Opcoes Jogar, jogo, i, n) =
   (Opcoes Sair, jogo, i, n)
event (EventKey (SpecialKey KeyDown) Down _ _) (Opcoes Jogar, jogo, i, n) =
   (Opcoes Sair, jogo, i, n)
event (EventKey (SpecialKey KeyUp) Down _ _) (Opcoes Sair, jogo, i, n) =
   (Opcoes Jogar, jogo, i, n)
event (EventKey (SpecialKey KeyDown) Down _ _) (Opcoes Sair, jogo, i, n) =
   (Opcoes Jogar, jogo, i, n)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Opcoes Sair, jogo, i, n) =
   error "Fim de Jogo"
-- Venceu Jogo
-- continuar a jogar depois de vencer
event (EventKey (SpecialKey KeyEnter) Down _ _) (VenceuJogo, jogo, i, n) =
   (Opcoes Jogar, jogo, i, n)
-- identificar que venceu Jogo
event _ (ModoJogo, (Jogo (x, y) []), i, n) = (VenceuJogo, (Jogo (x, y) []), i, n)
-- Modo Jogo
event (EventKey (SpecialKey KeyUp) Down _ _) (ModoJogo, (Jogo (x, y) l), i, n) =
    (ModoJogo, newState (x, y + 50) l, i, n)
event (EventKey (SpecialKey KeyDown) Down _ _) (ModoJogo, (Jogo (x, y) l), i, n) =
    (ModoJogo, newState (x, y - 50) l, i, n)
event (EventKey (SpecialKey KeyLeft) Down _ _) (ModoJogo, (Jogo (x, y) l), i, n) =
    (ModoJogo, newState (x - 50, y) l, i, n)
event (EventKey (SpecialKey KeyRight) Down _ _) (ModoJogo, (Jogo (x, y) l), i, n) =
    (ModoJogo, newState (x + 50, y) l, i, n)
-- não fazer nada em outros casos    
event _ w = w

time :: Float -> World -> World
time t (o, j, i, n) = (o, j, i, n+t)

main :: IO ()
main = do
  pac_open <- loadBMP "pac_open.bmp"
  pac_closed <- loadBMP "pac_closed.bmp"
  let images = [scale 1.5 1.5 pac_open, scale 2 2 pac_closed]
  play window (greyN 0.5) fr (initialState images) drawState event time