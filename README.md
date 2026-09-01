# Minyeong Yoon's Academic Website

This repository contains the source files and rendered output for my personal academic website:

**Website:** https://minyeongyoon.github.io

I am a Ph.D. Candidate in Criminology at the University of Texas at Dallas. This website presents my research interests, publications, teaching experience, honors and awards, and curriculum vitae.

## Built With

The website was created using:

* [Quarto](https://quarto.org/)
* RStudio
* GitHub Pages

## Repository Structure

* `_quarto.yml`: Website configuration and navigation
* `index.qmd`: Home page
* `research.qmd`: Research and publications
* `teaching.qmd`: Teaching philosophy and experience
* `awards.qmd`: Honors, awards, and fellowships
* `cv.qmd`: Curriculum vitae page
* `styles.css`: Custom website styling
* `docs/`: Rendered website files published through GitHub Pages

## Rebuilding the Website

To rebuild or update this website:

1. Clone or download this repository.
2. Open the `.Rproj` file in RStudio.
3. Make sure Quarto is installed and available in RStudio.
4. Edit the relevant `.qmd` source files.
5. Save the changes.
6. Click **Render Website** in RStudio, or run the following command in the Terminal:

```bash
quarto render
```

7. Confirm that the updated HTML files have been generated in the `docs/` directory.
8. Review the changes in GitHub Desktop.
9. Commit the source files and the updated `docs/` files with a meaningful commit message.
10. Push the commit to the `main` branch on GitHub.

GitHub Pages publishes the website from the `docs/` directory of the `main` branch.

## Data and Privacy

This repository does not contain confidential research data, personally identifiable information, passwords, or authentication credentials.

## License

The website source code and configuration files are available under the [MIT License](LICENSE).
All personal photographs, CV materials, publication information, and original written content are © 2026 Minyeong Yoon and are not covered by the MIT License unless otherwise noted. 
