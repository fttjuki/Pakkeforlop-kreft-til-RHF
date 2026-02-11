# Config: USER_local.R

## Hvorfor dette mønsteret?
I et **public repo** ønsker vi at ingen interne stier/servere skal havne i git.
Derfor:
- `config/USER_example.R` ligger i git og har kun **plassholdere**.
- Du lager `config/USER_local.R` lokalt med dine ekte interne stier.
- `USER_local.R` ignoreres av git via `.gitignore`.

## Slik gjør du
1. Kopier: `config/USER_example.R` → `config/USER_local.R`
2. Åpne `config/USER_local.R` og fyll inn:
   - `base_dir`, `app_dir`, `out_dir`
3. Kjør `scripts/00_run.R`
