# Depositing this repository

## 1. Run it first

```r
Rscript install_deps.R
Rscript run_all.R
```

Check the printed output against the numbers in the manuscript. The base case and
the tornado are analytic and must match to the last digit. The probabilistic
results will be extremely close but not bit-identical to any run made with a
different random number generator — the manuscript's PSA figures must come from
**this** R run.

## 2. Fill in the placeholders

- `CITATION.cff` — your name
- `DESCRIPTION` — your name
- Manuscript, "Data and code availability" — insert the Zenodo DOI once minted

## 3. Push to GitHub

```bash
cd kassep-cea
git init
git add .
git commit -m "KASSEP cost-effectiveness analysis: R code and data"
git branch -M main
git remote add origin https://github.com/babamaiyaki200016-prog/kassep-cea.git
git push -u origin main
```

## 4. Mint a DOI with Zenodo

1. Sign in at https://zenodo.org with your GitHub account.
2. Under **GitHub**, switch on `kassep-cea`.
3. Back on GitHub, create a release (`v1.0.0`). Zenodo archives it and issues a DOI.
4. Put the DOI badge in `README.md` and the DOI itself in the manuscript.

A GitHub URL can be deleted; a DOI cannot. Journals increasingly require the DOI,
so do step 4.

## 5. Sentence for the manuscript

> The analysis code is available at https://github.com/babamaiyaki200016-prog/kassep-cea
> and archived at Zenodo (DOI: 10.5281/zenodo.XXXXXXX).
