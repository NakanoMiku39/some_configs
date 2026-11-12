# some_configs

Dotfiles arranged as GNU stow packages. Each top-level directory is a package;
paths inside a package are relative to `$HOME`.

## Packages

- `aerospace`
- `alacritty`
- `emacs`
- `foot`
- `git`
- `gtk`
- `i3blocks`
- `i3status`
- `i3wm`
- `niri`
- `noctalia`
- `picom`
- `scripts`
- `starship`
- `swaylock`
- `systemd`
- `tmux`
- `vim`
- `zsh`

The `emacs` package links the regular `~/.emacs.d` setup.  The legacy
Spacemacs-style `~/.spacemacs` file is kept out of Stow so the two mutually
exclusive Emacs configurations are not linked at the same time.
It uses Fira Code with programming ligatures, Noto Sans Mono CJK SC for CJK
text, and Noto Color Emoji for emoji.

The `zsh` package links `~/.zshrc` and `~/.p10k.zsh`. Third-party Oh My Zsh
plugins and themes are installed separately under `~/.oh-my-zsh/custom`.

The `niri` config starts Noctalia and delegates the launcher, notifications,
lock screen, wallpaper, volume, brightness, and media controls to it. Noctalia
must be installed separately. Its package enables the built-in Polkit agent.

In a normal niri desktop session, `Mod` is Super. Application shortcuts include:

- `Mod+D` or `Alt+Space`: Noctalia launcher
- `Mod+T`: Foot
- `Mod+B`: Brave
- `Mod+E`: Neomacs
- `Mod+N`: Noctalia notification center
- `Super+Alt+L`: Noctalia lock screen

The `foot` package links `~/.config/foot/foot.ini`. The niri config uses
`Mod+T` to launch foot. Alacritty remains available as an alternative terminal.
Foot uses Fira Code, Noto Sans Mono CJK SC, and Noto Color Emoji.

The `tmux` package uses Gruvbox colors for its status bar, pane borders, and
messages.

OneDriver, OpenClaw, and Hermes Gateway user services and their enablement
links are local-only and excluded from version control.

The `gtk` package links `~/.config/gtk-3.0/settings.ini` to remove GTK tooltip
delay.

Legacy desktop stacks are maintained on dedicated branches:

- `waybar-mako-fuzzel`
- `waybar-swaync-walker`

## RUN THIS
```sh
stow -v -t "$HOME" alacritty emacs foot git gtk niri noctalia picom \
  scripts swaylock systemd tmux zsh
```

## Usage

Preview links before changing `$HOME`:

```sh
stow -nv -t "$HOME" aerospace alacritty emacs foot git gtk i3blocks \
  i3status i3wm niri noctalia picom scripts starship swaylock systemd \
  tmux vim zsh
```

Apply the links:

```sh
stow -v -t "$HOME" aerospace alacritty emacs foot git gtk i3blocks \
  i3status i3wm niri noctalia picom scripts starship swaylock systemd \
  tmux vim zsh
```

Remove links managed by stow:

```sh
stow -Dv -t "$HOME" aerospace alacritty emacs foot git gtk i3blocks \
  i3status i3wm niri noctalia picom scripts starship swaylock systemd \
  tmux vim zsh
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
