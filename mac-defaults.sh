# Need to logout and log back in order for these to take effect.

# Increase key repeat speeds beyond which they can be set in the settings UI
# https://apple.stackexchange.com/questions/10467/how-to-increase-keyboard-key-repeat-rate-on-os-x
defaults write -g InitialKeyRepeat -int 12 # normal maximum is 15 (225 ms) through UI
defaults write -g KeyRepeat -int 1 # normal minimum is 2 (30 ms) through UI

# Speed up trackpad tracking speed
# https://apple.stackexchange.com/questions/15210/how-to-increase-tracking-speed-beyond-the-maximum
defaults write -g com.apple.trackpad.scaling -float 6.0

# Speed up mouse tracking speed
# Note that this gets reset if you change the mouse speed via the slider in Settings.app
# https://apple.stackexchange.com/questions/15210/how-to-increase-tracking-speed-beyond-the-maximum
defaults write -g com.apple.mouse.scaling -float 6.0
