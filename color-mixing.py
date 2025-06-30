

from PIL import ImageColor

# convert hex to rgb
purplish = '#e366ca'
rgb_purplish = ImageColor.getcolor(purplish, "RGB")

lightblue = '#039BE5'
rgb_lightblue = ImageColor.getcolor(lightblue, "RGB")

lightgreen = '#7CB342'
rgb_lightgreen = ImageColor.getcolor(lightgreen, "RGB")

# mix the colors
mix = tuple([int((a+b)/2) for a, b in zip(rgb_purplish, rgb_lightblue)])
# convert to hex
hex_mix_purpblue = '#%02x%02x%02x' % mix

# mix blue with grey
rgb_gray = ImageColor.getcolor('#cccccc', "RGB")
rgb_gray = ImageColor.getcolor('#eeeeee', "RGB")
rgb_purpblue = ImageColor.getcolor(hex_mix_purpblue, "RGB")
# mix the colors
mix = tuple([int((a+b)/2) for a, b in zip(rgb_gray, rgb_purpblue)])
# convert to hex
hex_mix = '#%02x%02x%02x' % mix
hex_mix

# mix the colors
mix = tuple([int((a+b)/2) for a, b in zip(rgb_gray, rgb_lightgreen)])
# convert to hex
hex_mix = '#%02x%02x%02x' % mix
hex_mix