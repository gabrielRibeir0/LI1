module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Exit

data Menu = Save | ModoJogo deriving (Show, Read, Eq)

-- Estado do Gloss com menu, coordenadas do pacman,
-- duas imagens e um valor de segundos passados desde o início do programa
type Estado = (Menu, (Float, Float))
        
type EstadoGloss = (Estado, (Picture, Picture, Float))

-- Inicialização do estado com os valores anteriormente guardados em ficheiro
estadoInicial :: (Float,Float) -> Estado 
estadoInicial c = (ModoJogo, c)

estadoGlossInicial :: Picture -> Picture -> ((Float,Float),Float) -> EstadoGloss
estadoGlossInicial p1 p2 (c,t)= (estadoInicial c, (p1, p2, t))

-- Modo de Jogo
reageEventoGloss :: Event -> EstadoGloss -> IO EstadoGloss 
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _) ((ModoJogo, (x,y)), e) =
      return $ ((ModoJogo, (x,y+5)), e)
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) ((ModoJogo, (x,y)), e) =
      return $ ((ModoJogo, (x,y-5)), e)
reageEventoGloss (EventKey (SpecialKey KeyLeft) Down _ _) ((ModoJogo, (x,y)), e) =
      return $  ((ModoJogo, (x-5,y)), e)
reageEventoGloss (EventKey (SpecialKey KeyRight) Down _ _) ((ModoJogo, (x,y)), e) =
      return $ ((ModoJogo, (x+5,y)), e)
-- ao pressionar tecla "Space" suspende jogo e guarda estado
reageEventoGloss  (EventKey (SpecialKey KeySpace) Down _ _) ((ModoJogo, c), (p1,p2,t)) =
      do writeFile "save.txt" (show (c,t)) -- grava coordenadas e tempo em ficheiro
         return ((Save, c),(p1,p2,t)) -- suspende jogo 
-- retoma jogo  ao pressionar tecla "Space" a partir do estado anterior
reageEventoGloss  (EventKey (SpecialKey KeySpace) Down _ _) ((Save, c),e) =
      return ((ModoJogo,c), e)
-- termina jogo e guarda estado ao pressionar a tecla "q"
reageEventoGloss (EventKey (Char 'q') Down _ _) ((_, c), (_,_,t)) =     
      do writeFile "save.txt" (show (c,t))
         putStrLn "FIM"
         exitSuccess
reageEventoGloss _ w = return w


reageTempoGloss :: Float -> EstadoGloss -> IO EstadoGloss 
reageTempoGloss n ((ModoJogo, (x,y)), (p1, p2, b)) =
      return $ ((ModoJogo, (x,y)), (p1, p2, b+n)) 
reageTempoGloss n ((Save, (x,y)), e) =
      return $ ((Save, (x,y)), e) -- efeito do tempo suspenso

fr :: Int 
fr = 50

dm :: Display
dm = InWindow "Novo Jogo" (400, 400) (0, 0)

-- alternar a imagem a cada 100 milissegundos
desenhaEstadoGloss :: EstadoGloss -> IO Picture
desenhaEstadoGloss ((_, (x,y)), (p1, p2, b))
 | (mod (round (b*1000)) 200) < 100 = return $ Translate x y p1
 | otherwise = return $ Translate x y p2


--- usa playIO e inicializa estado com informação do estado anterior guardada em ficheiro
main :: IO ()
main = do
--  writeFile "save.txt" "((0,0),0)"
    p1 <- loadBMP "pac_open.bmp"
    p2 <- loadBMP "pac_closed.bmp"
    saved <- readFile "save.txt"
    let (coord, time) = (read saved)
    playIO dm                     
           (greyN 0.5)               
           fr                        
           (estadoGlossInicial p1 p2 (coord, time))
           desenhaEstadoGloss   
           reageEventoGloss  
           reageTempoGloss  