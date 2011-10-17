doubleMe x = x + x

doubleSmallNumber x = if x > 100
                      then x
                      else x*2


kgs_per_lb = 0.45359237
m_per_in = 0.0254
bmi weight height = weight * kgs_per_lb / ((height * m_per_in) ^ 2)

