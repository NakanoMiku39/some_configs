# some_configs

Dotfiles arranged as GNU stow packages. Each top-level directory is a package;
paths inside a package are relative to `$HOME`.

## Packages

- `aerospace`
- `alacritty`
- `emacs`
- `git`
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

## RUN THIS
```sh
stow -nv -t "$HOME" alacritty emacs git niri picom scripts swaylock systemd tmux waybar zsh
```

## Usage

Preview links before changing `$HOME`:

```sh
stow -nv -t "$HOME" aerospace alacritty emacs git i3blocks i3status i3wm niri picom scripts starship swaylock systemd tmux vim waybar zsh
```

Apply the links:

```sh
stow -v -t "$HOME" aerospace alacritty emacs git i3blocks i3status i3wm niri picom scripts starship swaylock systemd tmux vim waybar zsh
```

Remove links managed by stow:

```sh
stow -Dv -t "$HOME" aerospace alacritty emacs git i3blocks i3status i3wm niri picom scripts starship swaylock systemd tmux vim waybar zsh
```

If existing files in `$HOME` conflict with stow, move or back them up first,
then run the preview command again.

## Checks

GitHub Actions runs the repository secret check on pushes and pull requests.

Local commit checks use native Git hooks, so no extra plugin is required. Git
does not enable repository hooks automatically after clone; enable them once per
clone:

```sh
git config core.hooksPath .githooks
```

The pre-commit hook checks staged files before commit. The GitHub Action checks
tracked files in CI. Both call `.githooks/check-secrets`, which blocks likely
secrets, private keys, token files, password-store entries, and wallet files.
