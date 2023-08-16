# pm4net

[![GitHub](https://img.shields.io/github/license/pm4net/pm4net?style=flat-square)](https://github.com/pm4net/pm4net/blob/master/LICENSE)
[![GitHub Workflow Status](https://img.shields.io/github/actions/workflow/status/pm4net/pm4net/tests.yml?label=tests&style=flat-square&branch=master)](https://github.com/pm4net/pm4net/actions/workflows/tests.yml)
[![Nuget (with prereleases)](https://img.shields.io/nuget/vpre/pm4net?label=NuGet&style=flat-square)](https://www.nuget.org/packages/pm4net/)
[![Nuget](https://img.shields.io/nuget/dt/pm4net?label=NuGet%20Downloads&style=flat-square)](https://www.nuget.org/packages/pm4net/#versions-body-tab)

A general-purpose Process Mining library for .NET, primarily focusing on Object-Centric Event Logs (OCEL), using the [.NET OCEL library](https://github.com/pm4net/OCEL) to support the [Structured Log Explorer](https://github.com/pm4net/StructuredLogExplorer).

Features:
- Flattening of OCEL logs to single object types [2]
- Discovery of Directly-Follows Multigraphs [3, 4]
- Visualisation of DFM's using Graphviz

# References

[1] Farhang, A., Park, G. G., Berti, A., & Aalst, W. Van Der. (2020). OCEL Standard. http://ocel-standard.org/

[2] Van Der Aalst, W. M. P., & Berti, A. (2020). Discovering Object-Centric Petri Nets. Fundamenta Informaticae, 175(1–4), 1–40. https://doi.org/10.48550/arxiv.2010.02047

[3] Van Der Aalst, W. M. P. (2019). Limitations of the directly-follows graph. https://www.sciencedirect.com/science/article/pii/S1877050919322367

[4] van der Aalst, W. M. P. (2019). Object-Centric Process Mining: Dealing with Divergence and Convergence in Event Data. Lecture Notes in Computer Science (Including Subseries Lecture Notes in Artificial Intelligence and Lecture Notes in Bioinformatics), 11724 LNCS, 3–25. https://doi.org/10.1007/978-3-030-30446-1_1