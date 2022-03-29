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

fromChar :: Char -> Maybe Token
fromChar 'a' = Just T_A
fromChar 'b' = Just T_B
fromChar 'c' = Just T_C
fromChar 'd' = Just T_D
fromChar 'e' = Just T_E
fromChar 'f' = Just T_F
fromChar 'g' = Just T_G
fromChar 'h' = Just T_H
fromChar 'i' = Just T_I
fromChar 'j' = Just T_J
fromChar 'k' = Just T_K
fromChar 'l' = Just T_L
fromChar 'm' = Just T_M
fromChar 'n' = Just T_N
fromChar 'o' = Just T_O
fromChar 'p' = Just T_P
fromChar 'q' = Just T_Q
fromChar 'r' = Just T_R
fromChar 's' = Just T_S
fromChar 't' = Just T_T
fromChar 'u' = Just T_U
fromChar 'v' = Just T_V
fromChar 'w' = Just T_W
fromChar 'x' = Just T_X
fromChar 'y' = Just T_Y
fromChar 'z' = Just T_Z
fromChar '0' = Just T_0
fromChar '1' = Just T_1
fromChar '2' = Just T_2
fromChar '3' = Just T_3
fromChar '4' = Just T_4
fromChar '5' = Just T_5
fromChar '6' = Just T_6
fromChar '7' = Just T_7
fromChar '8' = Just T_8
fromChar '9' = Just T_9
fromChar _   = Nothing

ord ::  Integral n => Token -> n
ord = fromIntegral . fromEnum
