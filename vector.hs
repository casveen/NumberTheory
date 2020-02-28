data Vector a = EmptyVector | Element a (Vector a) deriving (Show, Eq)

vectorFromList:: [a]   -> Vector a
vectorFromList []       = EmptyVector
vectorFromList (x:xs) = Element x (vectorFromList xs)

unravel :: Vector a -> [a]
unravel EmptyVector = []
unravel (Element e v) = e:(unravel v)

getVectorValue:: Vector a -> Int -> a
getVectorValue (Element e v) 0= e
getVectorValue EmptyVector i= error ("Index exceeded by "++(show (i+1)))
getVectorValue (Element e v) i= getVectorValue v (i-1)

vectorDot::(Num a) => Vector a -> Vector a -> a
vectorDot EmptyVector EmptyVector         = 0
vectorDot EmptyVector (Element e2 v2)     = error "vectors are not of same size"
vectorDot (Element e1 v1) EmptyVector     = error "vectors are not of same size"
vectorDot (Element e1 v1) (Element e2 v2) = e1*e2+vectorDot v1 v2

data NVector a = EmptyNVector | Size Int (Vector a) deriving (Show, Eq)
nvectorFromList:: [a]   -> NVector a
nvectorFromList []       = (Size 0) EmptyVector
nvectorFromList xs = (Size . length $ xs) (vectorFromList xs)

data Matrix a = EmptyMatrix | Row (Vector a) (Matrix a) deriving (Show, Eq)

matrixFromVectors:: [[a]] -> Matrix a
matrixFromVectors []         = EmptyMatrix
matrixFromVectors (vec:rest) = Row (vectorFromList vec) (matrixFromVectors rest)

getMatrixColumn:: Matrix a -> Int -> Vector a
getMatrixColumn (Row EmptyVector matrix) i   = EmptyVector
getMatrixColumn (Row r matrix) i             = (Element (getVectorValue r i)) (getMatrixColumn matrix i)

getMatrixColumns:: Matrix a -> [Vector a]
getMatrixColumns (Row r EmptyMatrix) = [Element e EmptyVector | e <- unravel r]
getMatrixColumns (Row r matrix)      = [Element e v| (e,v) <- zip (unravel r) (getMatrixColumns matrix)]

matrixProduct:: (Num a) => Matrix a-> Matrix a-> Matrix a
-- create one matrix row at a time
matrixProduct (Row r1 matrix1') EmptyMatrix     = error "empty matrix as second argument"
matrixProduct EmptyMatrix matrix2               = EmptyMatrix
matrixProduct (Row r1 matrix1') matrix2         = Row (vectorFromList [vectorDot r1 c | c <- getMatrixColumns matrix2 ]) (matrixProduct matrix1' matrix2)
