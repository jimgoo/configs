
## Add this to top of ~/.zshrc in order for TMUX to not set the path twice (breaks conda).
# if [ -x /usr/libexec/path_helper ]; then
#   PATH="" # Add this line
#   eval `/usr/libexec/path_helper -s`
# fi

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


## From: https://github.com/mathiasbynens/dotfiles/blob/master/.macos

# Always show scrollbars
defaults write NSGlobalDomain AppleShowScrollBars -string "Always"

# Expand save panel by default
defaults write NSGlobalDomain NSNavPanelExpandedStateForSaveMode -bool true
defaults write NSGlobalDomain NSNavPanelExpandedStateForSaveMode2 -bool true

# Expand print panel by default
defaults write NSGlobalDomain PMPrintingExpandedStateForPrint -bool true
defaults write NSGlobalDomain PMPrintingExpandedStateForPrint2 -bool true

# Disable automatic capitalization as it’s annoying when typing code
defaults write NSGlobalDomain NSAutomaticCapitalizationEnabled -bool false

# Disable smart dashes as they’re annoying when typing code
defaults write NSGlobalDomain NSAutomaticDashSubstitutionEnabled -bool false

# Disable automatic period substitution as it’s annoying when typing code
defaults write NSGlobalDomain NSAutomaticPeriodSubstitutionEnabled -bool false

# Disable smart quotes as they’re annoying when typing code
defaults write NSGlobalDomain NSAutomaticQuoteSubstitutionEnabled -bool false

# Disable auto-correct
# defaults write NSGlobalDomain NSAutomaticSpellingCorrectionEnabled -bool false

# Disable Gatekeeper
sudo spctl --master-disable
