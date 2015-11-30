bmiTell :: (RealFloat a) => a -> a-> String
bmiTell weight height
  | weight / height ^ 2 <= 18.5 = "You're under weight, you emo, you!"
  | weight / height ^ 2 <= 25.0 = "You're supposedly normal, Pffft, I bet you're ugly!"
  | weight / height ^ 2 <= 30.0 = "You're fat, Lose some weigth, fatty!"
  | otherwise                   = "You're a whale, congratulations!"
