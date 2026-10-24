# SYNERGY

## Team Members
- **Insang Lee**
- **Minyeong Yoon**

## Who Did What

### Insang Lee
- Developed **Chart 9: Interactive Scatter Plot** examining the relationship between GDP per capita and the Happy Planet Index (HPI).
- Added interactive features including the continent filter, country labels, hover information, and an optional trend line.
- Helped connect the Scatter and Bubble charts through a shared GDP range filter so that both charts use the same filtered subset of countries.
- Prepared the presentation slides and presentation materials.
- Created and organized the **SYNERGY.md** file documenting team contributions and the integration process.

### Minyeong Yoon
- Proposed the overall topic and visualization direction for the project using the Happy Planet Index data and helped shape how the three charts would work together.
- Developed **Chart 18: Interactive Bubble Chart** examining GDP per capita, life satisfaction, and ecological footprint, including interactive controls for bubble size, logarithmic GDP scaling, country labels, and hover information.
- Developed the additional **Contour Plot** to examine the distribution and concentration of countries across GDP per capita and HPI, including controls for contour levels, country points, labels, and a trend line.
- Organized the overall Shiny app structure and integrated the Scatter, Bubble, and Contour visualizations into a single application with separate navigation tabs.

## How We Combined Our Work

We first developed our assigned visualizations separately and then combined them into one Shiny application. We used the same Happy Planet Index dataset and coordinated the visual design, labels, fonts, and interactive features across the charts.

To meet the reactivity requirement, we connected the Scatter and Bubble charts to a shared GDP range filter. This allows both charts to display data from the same GDP subset while preserving their chart-specific interactive controls.

## What Was Better Together

Combining the three charts provides a more complete view of sustainable wellbeing than any single visualization alone. The Scatter Plot focuses on the relationship between GDP and HPI, the Bubble Chart adds life satisfaction and ecological footprint, and the Contour Plot shows where countries are concentrated within the GDP-HPI distribution.

The shared GDP filter also makes it easier for users to compare patterns across the Scatter and Bubble charts using the same group of countries.

## What We Would Do Differently

If we had more time, we would plan the shared reactive structure earlier in the development process rather than integrating it near the end. We would also further standardize the controls and styling across all three charts and explore additional cross-chart interactions.