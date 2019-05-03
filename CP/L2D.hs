

module L2D  where

import Cp

type L2D = X Caixa Tipo

data X a b = Unid a | Comp b (X a b) (X a b) deriving Show

data Tipo = V | Vd | Ve | H | Ht | Hb

{-

V - agregação vertical alinhada ao centro
Vd - agregação vertical justificada à direita
Ve - agregação vertical justificada à esquerda
H - agregação horizontal alinhada ao centro
Hb - agregação horizontal alinhada pela base
Ht - agregação horizontal alinhada pelo topo

-}

type Caixa = ((Int, Int), (Texto, G.Color ))
type Texto = String

type Fig = [(Origem, Caixa)]
type Origem = (Float, Float)

inL2D  = undefined
outL2D = undefined

cataL2D g   = g . recL2D (cataL2D g) . outL2D   

recL2D  f   = id -|- id >< f

anaL2D  g   = inL2D . recL2D (anaL2D g) . g

hyloL2D h g = cataL2D h . anaL2D g

baseL2D f g = id -|- f >< g

A = ((100, 200), ("A", col_blue))

(Comp  T (Unid B) (Comp T (Unid A) (Comp T (Unid C) .... ))))