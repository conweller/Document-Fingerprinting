module Token
  ( Token(..)
  , fromChar
  , ord
  ) where

data Token
  = T_A
  | T_B
  | T_C
  | T_D
  | T_E
  | T_F
  | T_G
  | T_H
  | T_I
  | T_J
  | T_K
  | T_L
  | T_M
  | T_N
  | T_O
  | T_P
  | T_Q
  | T_R
  | T_S
  | T_T
  | T_U
  | T_V
  | T_W
  | T_X
  | T_Y
  | T_Z
  | T_0
  | T_1
  | T_2
  | T_3
  | T_4
  | T_5
  | T_6
  | T_7
  | T_8
  | T_9
  deriving (Enum, Ord, Eq, Show, Bounded)


fromChar :: Char -> Token
fromChar 'a' = T_A
fromChar 'b' = T_B
fromChar 'c' = T_C
fromChar 'd' = T_D
fromChar 'e' = T_E
fromChar 'f' = T_F
fromChar 'g' = T_G
fromChar 'h' = T_H
fromChar 'i' = T_I
fromChar 'j' = T_J
fromChar 'k' = T_K
fromChar 'l' = T_L
fromChar 'm' = T_M
fromChar 'n' = T_N
fromChar 'o' = T_O
fromChar 'p' = T_P
fromChar 'q' = T_Q
fromChar 'r' = T_R
fromChar 's' = T_S
fromChar 't' = T_T
fromChar 'u' = T_U
fromChar 'v' = T_V
fromChar 'w' = T_W
fromChar 'x' = T_X
fromChar 'y' = T_Y
fromChar 'z' = T_Z
fromChar '0' = T_0
fromChar '1' = T_1
fromChar '2' = T_2
fromChar '3' = T_3
fromChar '4' = T_4
fromChar '5' = T_5
fromChar '6' = T_6
fromChar '7' = T_7
fromChar '8' = T_8
fromChar '9' = T_9
fromChar _ = error "character must be lowercase alphanumeric"

ord :: Token -> Int
ord = fromEnum
