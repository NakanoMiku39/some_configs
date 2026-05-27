# some_configs

Dotfiles arranged as GNU stow packages. Each top-level directory is a package;
paths inside a package are relative to `$HOME`.

## Packages

- `aerospace`
- `alacritty`
- `emacs`
- `i3blocks`
- `i3status`
- `i3wm`
- `niri`
- `picom`
- `scripts`
- `starship`
- `swaylock`
- `systemd`
- `tmux`
- `vim`
- `waybar`
- `zsh`

## Usage

Preview links before changing `$HOME`:

```sh
stow -nv -t "$HOME" aerospace alacritty emacs i3blocks i3status i3wm niri picom scripts starship swaylock systemd tmux vim waybar zsh
```

Apply the links:

```sh
stow -v -t "$HOME" aerospace alacritty emacs i3blocks i3status i3wm niri picom scripts starship swaylock systemd tmux vim waybar zsh
```

Remove links managed by stow:

```sh
stow -Dv -t "$HOME" aerospace alacritty emacs i3blocks i3status i3wm niri picom scripts starship swaylock systemd tmux vim waybar zsh
```

If existing files in `$HOME` conflict with stow, move or back them up first,
then run the preview command again.
