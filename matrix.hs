-- haskell implementation of a 2x2 matrix
data Matrix a = Matrix a a a a deriving (Eq, Show)

--define equality between 2x2 matrices, the elements must be equatable
instance (Eq m) => Eq (Matrix m) where
    (Matrix e1 e2 e3 e4 == Matrix e1' e2' e3' e4') = (e1 == e1') && (e2 == e2') && (e3 == e3') && (e4 == e4')
    _ = _ =False
