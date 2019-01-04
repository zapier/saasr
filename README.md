# saasr

A SaaS subscription simulator - a tool to learn about the statistical mechanics of subscriptions

<!-- badges: start -->
  [![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
  <!-- badges: end -->

# Why should I use it?

The purpose of the simulator is to help businesses that use a subscription business model learn about the mechanics of a subscription business model.

Subscription business models are a great choice for software businesses. Before subscriptions were used to finance software development, users typically paid an upfront fixed fee for software. As time went on bugs were inevitably found that required more development cost. Also, as users became familiar with the software they thought of new features that would enhance the value the software could produce. Unfortunately neither of these improvements are directly or easily financed under an upfront fixed fee business model.

Enter the subscription billing model. The innovation of using subscriptions to finance ongoing software development gave companies a way to finance bug fixes and feature improvements. The model recognizes the fact that software is developed progressively and that users benefit from the stability of continual improvement of software.

Further, subscriptions create an equitable relationship between software users and companies. If the company doesn't uphold its end of the bargain by continually fixing bugs and shipping features, the user can always revoke the subscription. This incentivizes companies to tend, grow and support software on an ongoing basis.

The premise of this simulator is that by learning the dynamics of the subscription business model, companies can finance the development software more efficiently. As subscriptions spread out the cost of developing software, innovations can lead to lower cost software per unit of value received by customers, a win-win for both users and companies.

# How do I use it?

The simulator is still a really young piece of software. As young software often contains bugs, you should beware when using the simulator. See a bug? Please open an issue.

```r
devtools::install_github("zapier/saasr")
library(saasr)
subscribers_by_day(cohorts = rep(1e2, 365*5), dist = "weibull", shape = 0.3, scale = 500)
```

```r
library(dplyr)
library(ggplot2)
subscribers_by_day(cohorts = rep(1e2, 365*5), dist = "weibull", shape = 0.3, scale = 500) %>%
    bind_rows(subscribers_by_day(cohorts = rep(1e2, 365*5), dist = "weibull", shape = 0.5, scale = 600)) %>%
    bind_rows(subscribers_by_day(cohorts = rep(1e2, 365*5), dist = "lnorm", meanlog = 5, sdlog = 2)) %>%
    ggplot(aes(x = day, y = subscribers, colour = factor(group))) +
    geom_point() +
    scale_y_continuous(labels = scales::comma) +
    scale_color_discrete(name = "Churn distribution") +
    theme(legend.text = element_text(size = 18)) +
    ylab("Subscribers") +
    ggtitle("SaaS Simulator subscribers") +
    xlab("Days from start of business")
```

![Visualization of subscribers](https://zappy.zapier.com/BCEDC82F-BBCA-40E1-8E04-C46CA0C52A7C.png)

```r
library(dplyr)
library(ggplot2)
library(ggfortify)
tibble:::tibble(time_to = rweibull(1e4, 0.3, 500),
                censor = 1) %>%
  mutate(group = "weibull(0.3, 500)") %>%
  bind_rows(tibble:::tibble(time_to = rweibull(1e4, 0.5, 600),
                            censor = 1) %>%
              mutate(group = "weibull(0.5, 600)"),
            tibble:::tibble(time_to = rlnorm(1e4, 5, 2),
                            censor = 1) %>%
              mutate(group = "lnorm(5, 2)")
  ) %>%
  survival::survfit(survival::Surv(time_to, censor) ~ factor(group), data = .) %>%
  ggplot2::autoplot() +
  coord_cartesian(xlim = c(0, 365)) +
  ylab("Share of cohort remaining") +
  xlab("Days from subscription purchase") +
  ggtitle("Subscription churn (survival function)")
```

![Subscription curve survival function](https://zappy.zapier.com/A0A138DE-52CF-4619-909C-9715B0B920E8.png)











