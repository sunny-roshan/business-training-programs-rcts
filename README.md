# Bayesian Hierarchical Meta-analysis of Business Training Programs
_This project was the basis for my Master's thesis._

Are entrepreneurs born, or can they be made? The success of billions of dollars’ worth of development aid hinges on the answer to this question. A large fraction of the labour
force in developing countries consists of 'reluctant entrepreneurs' (Banerjee et al. 2015), rather than innate innovators: owing to poverty and dysfunctional labour markets,
they are unable to find steady wage work, and are instead forced to run micro- and small enterprises to earn a living. These small firms tend to perform poorly, earning
negligible profits, and often fail to employ basic business management practices that are near-universal in the developed world, such as accounting, bookkeeping, and inventory
management. Business training programs have become common across the developing world over the last two decades: by equipping small business owners with business skills, these programs aim to create a pathway out of poverty while also helping generate employment and drive economy-wide growth.  

However, the empirical literature has not yet answered a key question: _do these programs actually work?_ Can these reluctant entrepreneurs be put on the road to growth and profitability?  

This project employs Bayesian hierarchical modelling, a methodology for evidence aggregation that is common in medicine but relatively new in development economics, to try to answer this question. I synthesise the evidence from twenty experimental evaluations of business training programs in fourteen developing countries to estimate the likely ‘true’ average effect of these programs. The Bayesian hierarchical model allows for the direct modelling of the heterogeneity and variability in the existing evidence, as well as for the modelling of relationships between observable variables and treatment effects.  

**Technical steps**
1. Load Stan for Bayesian inference.
2. Full study datasets have been provided for free for 5 studies (open science!). Each of these datasets is loaded and cleaned and their key variables (treatment, sales, profits) are harmonised.
3. For 17 studies, only headline summary statistics are available. These headline figures are loaded into a single dataset.
4. Fit a variety of specifications of Bayesian Hierarchical Models to the (simpler) dataset with headline results. Test out a variety of prior assumptions, evaluate model fit and posterior predictions. Do this separately for profits and sales.
5. Fit a Bayesian Hierarchical Model to the (more complicated) dataset with the full observational data from the five studies. Do this separately for profits and sales.
