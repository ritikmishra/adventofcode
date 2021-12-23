{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module VectorMath where

-- Stores x, y, z
newtype Vec3 = Vec3 (Int, Int, Int) deriving (Show, Eq, Ord)

-- Stores rows
newtype Mat3 = Mat3 (Vec3, Vec3, Vec3) deriving (Eq)

instance Show Mat3 where
  show (Mat3 (r1, r2, r3)) = "\n[ " ++ show r1 ++ "\n  " ++ show r2 ++ "\n  " ++ show r3 ++ " ]"

transpose :: Mat3 -> Mat3
transpose
  ( Mat3
      ( Vec3 (a11, a12, a13),
        Vec3 (a21, a22, a23),
        Vec3 (a31, a32, a33)
        )
    ) =
    Mat3
      ( Vec3 (a11, a21, a31),
        Vec3 (a12, a22, a32),
        Vec3 (a13, a23, a33)
      )

class Subtractable a where
  sub :: a -> a -> a

instance (Addable a, Multipliable Int a a) => Subtractable a where
  sub lhs rhs = lhs `add` ((-1 :: Int) `mul` rhs) 
    

magnitude :: Vec3 -> Float
magnitude (Vec3 (x, y, z)) =
    let (x', y', z') = (fromIntegral x, fromIntegral y, fromIntegral z)
    in sqrt (x' * x' + y' * y'  + z' * z')

-- instance (Multipliable Int t t) => Subtractable t where
--   sub lhs rhs = lhs `add` ((-1 :: Int) `mul` rhs)

class Addable a where
  add :: a -> a -> a

instance Addable Vec3 where
  add (Vec3 (a1, a2, a3)) (Vec3 (b1, b2, b3)) = Vec3 (a1 + b1, a2 + b2, a3 + b3)

instance Addable Mat3 where
  add (Mat3 (ar1, ar2, ar3)) (Mat3 (br1, br2, br3)) = Mat3 (ar1 `add` br1, ar2 `add` br2, ar3 `add` br3)

class Multipliable a b c | a b -> c where
  mul :: a -> b -> c

instance Multipliable Vec3 Vec3 Int where
  mul (Vec3 (a1, a2, a3)) (Vec3 (b1, b2, b3)) = a1 * b1 + a2 * b2 + a3 * b3

instance Multipliable Mat3 Vec3 Vec3 where
  mul (Mat3 (row1, row2, row3)) vec = Vec3 (row1 `mul` vec, row2 `mul` vec, row3 `mul` vec)

instance Multipliable Mat3 Mat3 Mat3 where
  mul (Mat3 (ar1, ar2, ar3)) b =
    Mat3
      ( Vec3 (ar1 `mul` bc1, ar1 `mul` bc2, ar1 `mul` bc3),
        Vec3 (ar2 `mul` bc1, ar2 `mul` bc2, ar2 `mul` bc3),
        Vec3 (ar3 `mul` bc1, ar3 `mul` bc2, ar3 `mul` bc3)
      )
    where
      Mat3 (bc1, bc2, bc3) = transpose b

instance Multipliable Int Vec3 Vec3 where
  mul n (Vec3 (a1, a2, a3)) = Vec3 (n * a1, n * a2, n * a3)

instance Multipliable Int Mat3 Mat3 where
  mul n (Mat3 (r1, r2, r3)) = Mat3 (n `mul` r1, n `mul` r2, n `mul` r3)

eye :: Mat3
eye =
  Mat3
    ( Vec3 (1, 0, 0),
      Vec3 (0, 1, 0),
      Vec3 (0, 0, 1)
    )

rotX90 :: Mat3
rotX90 =
  Mat3
    ( Vec3 (1, 0, 0),
      Vec3 (0, 0, -1),
      Vec3 (0, 1, 0)
    )

rotY90 :: Mat3
rotY90 =
  Mat3
    ( Vec3 (0, 0, -1),
      Vec3 (0, 1, 0),
      Vec3 (1, 0, 0)
    )

rotZ90 :: Mat3
rotZ90 =
  Mat3
    ( Vec3 (0, -1, 0),
      Vec3 (1, 0, 0),
      Vec3 (0, 0, 1)
    )

all3DRotations :: [Mat3]
all3DRotations =
  [ eye,
    rotX90,
    rotX90 `mul` rotX90,
    rotX90 `mul` rotX90 `mul` rotX90,
    rotZ90,
    rotX90 `mul` rotZ90,
    rotX90 `mul` rotX90 `mul` rotZ90,
    rotX90 `mul` rotX90 `mul` rotX90 `mul` rotZ90,
    rotZ90 `mul` rotZ90,
    rotX90 `mul` rotZ90 `mul` rotZ90,
    rotX90 `mul` rotX90 `mul` rotZ90 `mul` rotZ90,
    rotX90 `mul` rotX90 `mul` rotX90 `mul` rotZ90 `mul` rotZ90,
    rotZ90 `mul` rotZ90 `mul` rotZ90,
    rotX90 `mul` rotZ90 `mul` rotZ90 `mul` rotZ90,
    rotX90 `mul` rotX90 `mul` rotZ90 `mul` rotZ90 `mul` rotZ90,
    rotX90 `mul` rotX90 `mul` rotX90 `mul` rotZ90 `mul` rotZ90 `mul` rotZ90,
    rotY90,
    rotX90 `mul` rotY90,
    rotX90 `mul` rotX90 `mul` rotY90,
    rotX90 `mul` rotX90 `mul` rotX90 `mul` rotY90,
    rotY90 `mul` rotY90 `mul` rotY90,
    rotX90 `mul` rotY90 `mul` rotY90 `mul` rotY90,
    rotX90 `mul` rotX90 `mul` rotY90 `mul` rotY90 `mul` rotY90,
    rotX90 `mul` rotX90 `mul` rotX90 `mul` rotY90 `mul` rotY90 `mul` rotY90
  ]

data Transformation = Transformation
  { rot :: Mat3,
    translation :: Vec3
  }

applyTrans :: Transformation -> Vec3 -> Vec3
applyTrans Transformation {rot = rot, translation = t} x =
  (transpose rot `mul` x) `add` t