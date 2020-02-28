-- haskell implementation of a 2x2 matrix
data Matrix a = Matrix a a a a deriving (Show)

--define equality between 2x2 matrices, the elements must be equatable
instance (Eq m) => Eq (Matrix m) where
    --((Matrix e1 e2 e3 e4) == (Matrix e1' e2' e3' e4')) = (e1 == e1') && (e2 == e2') && (e3 == e3') && (e4 == e4')
    (Matrix a1 a2 a3 a4) == ( Matrix b1 b2 b3 b4 ) = (a1==b1) && (a2==b2) && (a3==b3) &&(a4==b4)

unravel :: Matrix a -> [a]
unravel (Matrix a1 a2 a3 a4) = [a1,a2,a3,a4]

vzip :: (a->a->a) -> Matrix a -> Matrix a -> Matrix a
vzip f (Matrix a1 a2 a3 a4) (Matrix b1 b2 b3 b4) = Matrix (f a1 b1) (f a2 b2) (f a3 b3) (f a4 b4)

instance (Num m) => Num (Matrix m) where
    --(+) (Matrix a1 a2 a3 a4) (Matrix b1 b2 b3 b4) =
    --     Matrix (a1+b1) (a2+b2) (a3+b3) (a4+b4)
    (+) = vzip (+)
    (*) (Matrix a1 a2 a3 a4) (Matrix b1 b2 b3 b4) =
         Matrix (a1*b1+b2*b3) (a1*b2+a2*b4) (a3*b1+a4*b3) (a3*b2+a4*b4)



identity = Matrix 1 0 0 1
