# some_configs

Dotfiles arranged as GNU stow packages. Each top-level directory is a package;
paths inside a package are relative to `$HOME`.

## Packages

- `aerospace`
- `alacritty`
- `emacs`
- `foot`
- `fuzzel`
- `git`
- `gtk`
- `i3blocks`
- `i3status`
- `i3wm`
- `mako`
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

The `emacs` package links the regular `~/.emacs.d` setup.  The legacy
Spacemacs-style `~/.spacemacs` file is kept out of Stow so the two mutually
exclusive Emacs configurations are not linked at the same time.

The `zsh` package links `~/.zshrc` and `~/.p10k.zsh`. Third-party Oh My Zsh
plugins and themes are installed separately under `~/.oh-my-zsh/custom`.

The `fuzzel` package links `~/.config/fuzzel/fuzzel.ini`. The niri config uses
`Mod+D` to launch fuzzel.

The `foot` package links `~/.config/foot/foot.ini`. The niri config uses
`Mod+T` to launch foot. Alacritty remains available as an alternative terminal.

The `mako` package links `~/.config/mako/config`. The niri config starts mako
at session startup.

The `gtk` package links `~/.config/gtk-3.0/settings.ini` to remove GTK tooltip
delay for Waybar and other GTK3 surfaces.

## RUN THIS
```sh
stow -v -t "$HOME" alacritty emacs foot fuzzel git gtk mako niri picom scripts swaylock systemd tmux waybar zsh
```

## Usage

Preview links before changing `$HOME`:

```sh
stow -nv -t "$HOME" aerospace alacritty emacs foot fuzzel git gtk i3blocks i3status i3wm mako niri picom scripts starship swaylock systemd tmux vim waybar zsh
```

Apply the links:

```sh
stow -v -t "$HOME" aerospace alacritty emacs foot fuzzel git gtk i3blocks i3status i3wm mako niri picom scripts starship swaylock systemd tmux vim waybar zsh
```

Remove links managed by stow:

```sh
stow -Dv -t "$HOME" aerospace alacritty emacs foot fuzzel git gtk i3blocks i3status i3wm mako niri picom scripts starship swaylock systemd tmux vim waybar zsh
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
