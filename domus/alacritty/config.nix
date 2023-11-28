colors: with colors; ''
window:
  decorations: none

  # Prefer resizing window by discrete steps equal to cell dimensions.
  resize_increments: true


font:
  normal:
    family: BlexMono Nerd Font
    #style: Regular

  bold:
    family: BlexMono Nerd Font
    #style: Bold

  italic:
    family: BlexMono Nerd Font
    #style: Italic

  bold_italic:
    family: BlexMono Nerd Font
    #style: Bold Italic

  size: 14.0


colors:
  primary:
    background: '${black}'
    foreground: '${white}'
    #dim_foreground: '#828482'
    bright_foreground: '${whiteBright}'

  normal:
    black:   '${black}'
    red:     '${red}'
    green:   '${green}'
    yellow:  '${yellow}'
    blue:    '${blue}'
    magenta: '${magenta}'
    cyan:    '${cyan}'
    white:   '${white}'

  bright:
    black:   '${blackBright}'
    red:     '${redBright}'
    green:   '${greenBright}'
    yellow:  '${yellowBright}'
    blue:    '${blueBright}'
    magenta: '${magentaBright}'
    cyan:    '${cyanBright}'
    white:   '${whiteBright}'

  #dim:
  #  black:   '#131415'
  #  red:     '#864343'
  #  green:   '#777c44'
  #  yellow:  '#9e824c'
  #  blue:    '#556a7d'
  #  magenta: '#75617b'
  #  cyan:    '#5b7d78'
  #  white:   '#828482'


#bell:
  # Visual Bell Animation
  #
  # Animation effect for flashing the screen when the visual bell is rung.
  #
  # Values for `animation`:
  #   - Ease
  #   - EaseOut
  #   - EaseOutSine
  #   - EaseOutQuad
  #   - EaseOutCubic
  #   - EaseOutQuart
  #   - EaseOutQuint
  #   - EaseOutExpo
  #   - EaseOutCirc
  #   - Linear
  #animation: EaseOutExpo

  # Duration of the visual bell flash in milliseconds. A `duration` of `0` will
  # disable the visual bell animation.
  #duration: 0

  # Visual bell animation color.
  #color: '#ffffff'

  # Bell Command
  #
  # This program is executed whenever the bell is rung.
  #
  # When set to `command: None`, no command will be executed.
  #
  # Example:
  #   command:
  #     program: notify-send
  #     args: ["Hello, World!"]
  #
  #command: None


cursor:
  style:
    shape: Block
    blinking: Off
  unfocused_hollow: true


# Live config reload (changes require restart)
# Useless since config is built by nix
live_config_reload: false


# Offer IPC using `alacritty msg` (unix only)
ipc_socket: false


key_bindings:
  # Unset default font size modifiers
  - { key: Key0,           mods: Control, action: ReceiveChar }
  - { key: Equals,         mods: Control, action: ReceiveChar }
  - { key: Plus,           mods: Control, action: ReceiveChar }
  - { key: NumpadAdd,      mods: Control, action: ReceiveChar }
  - { key: Minus,          mods: Control, action: ReceiveChar }
  - { key: NumpadSubtract, mods: Control, action: ReceiveChar }
''
