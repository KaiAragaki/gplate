# Thinking about margins
# If margin = 1 then....

# Each section should have 1 well around it that is free This could be the same
# has having a section with one larger in each axis first, then somehow removing
# the top and left cols of each section (if tl....I think the cols removed
# depend on start_corner.)

# I'm beginning to rethink this. Margin 1 should remove every row/col around each section.

# If you want to remove only 1 around each one, you can do c(1, 1, 0, 0) or whatever.

# This is conceptually simpler as well as programatically simpler

# Each section should keep track of what wells of it are margin or not

# Unless this is automatically kept along the way, we don't have to remember if it is 'margin top' or not
# That doesn't make sense anyway because corner wells can be both
# It just makes sense to have an 'is_margin' col

