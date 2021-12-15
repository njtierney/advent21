library(tidyverse)

depths <- read_csv("data/day_1.txt",
                   col_names = c("depth_m"),
                   col_types = list(depth_m = col_integer()))

depths

# part 1
# Figure out how quickly the depth increases, just so you know what you're
# dealing with - you never know if the keys will get carried into deeper water
# by an ocean current or a fish or something.

# To do this, count the number of times a depth measurement increases from the
# previous measurement. (There is no measurement before the first measurement.)

depths_increase <- depths %>%
  mutate(depth_diff = c(NA, diff(depth_m)),
         depth_increase = depth_diff > 0)

depths_increase

sum(depths_increase$depth_increase, na.rm = TRUE)

# part 2
library(slider)

# Start by comparing the first and second three-measurement windows. The
# measurements in the first window are marked A (199, 200, 208); their sum is
# 199 + 200 + 208 = 607. The second window is marked B (200, 208, 210); its
# sum is 618. The sum of measurements in the second window is larger than the
# sum of the first, so this first comparison increased.
#
# Your goal now is to count the number of times the sum of measurements in
# this sliding window increases from the previous sum. So, compare A with B,
# then compare B with C, then C with D, and so on. Stop when there aren't
# enough measurements left to create a new three-measurement sum.

depths_increase_slide_3 <- depths_increase %>%
  mutate(
    depth_sum_slide_3 = slide_int(.x = depth_m,
                                  .f = sum,
                                  .after = 2,
                                  .complete = TRUE),
    diff_slide_3 = c(NA, diff(depth_sum_slide_3)),
    depth_slide_3_increase = diff_slide_3 > 0
  )

sum(depths_increase_slide_3$depth_slide_3_increase, na.rm = TRUE)
