# SH Planning System

SH is a software system designed to solve complex planning problems in various domains. It is based on state-based Hierarchical Task Network (HTN) planning and relies on a version of the Hierarchical Planning Definition Language (HDPL) for specifying planning problems. What sets SH apart is its modular and service-oriented architecture, providing flexibility, extensibility, and maintainability. The SH planning system offers versatile capabilities, including modular parsing of planning problems, quick variable binding and predicate grounding on the fly during plan generation, resource-efficient plan generation, and seamless integration into other larger systems. These capabilities make SH suitable for addressing real-world challenges in planning, automation, assistance, and decision-making.

## Getting Started

### Prerequisites

- JDK 1.8
- Scala 2.11.12
- sbt 
- Dependencies specified in `build.sbt`

### Installation
Clone the repository and build the project using `sbt` to prepare the codebase for use.

### Usage

#### Using `Client` object

To run a test case, use the `PlanningServices.planWithGivenDomainAndProblemNamePrintPlans` service within the `Client` object with the following parameters:

1. Domain name
2. Problem name
3. Number of plans to be generated

Ensure that the domain name and problem name match their corresponding filenames in the `resources/repository/<domainName>` directory. The generated plan, along with performance statistics, will be displayed in the terminal.

### Example

Execute the `Client` object with:
```scala
PlanningServices.planWithGivenDomainAndProblemNamePrintPlans("deployment", "p-dashboard", 1)
```

**Expected output:**

```    
Plan 1:
1) createinstance(dashboard)
2) createinstance(apache2)
3) start(1.0)
4) bind(httpd,0.0,1.0)
5) start(0.0)
6) createinstance(cassandra)
7) start(2.0)
8) run(2.0)
9) bind(cass-up,0.0,2.0)
10) run(0.0)

-------------------------------------------* Statistics *-------------------------------------------
Number of found plans: 1
First plan length: 10
*************************************************
Parsing time: 968.0 ms
Plan-generation time: 63.0 ms
Memory usage: 133.169152 MB
*************************************************
Number of evaluated states: 34
Number of decomposed tasks: 24
Number of recursions: 36
----------------------------------------------------------------------------------------------------
```

## Integration

### As a Library

Create and import a jar of the SH Planner System. Once imported, the following methods from the `PlanningServices` object are available for use:

- `planWithProvidedProblemInString(problemToBeSolved: String, numberOfPlans: Int = 1)`
- `storeProvidedDomainAndProblemInString(domainToBeSolved: String, problemToBeSolved: String)`
- `planWithProvidedDomainAndProblemInString( domainToBeSolved: String,problemToBeSolved: String, numberOfPlans: Int = 1)`
- `checkProvidedProblemInString(problemToBeChecked: String)`
- `checkProvidedDomainInString(domainToBeChecked: String)`
- `planWithGivenDomainAndProblemNameReturnString(domainName: String, problemName: String, numberOfPlans: Int = 1)` 

Note: The domain and problem files need to be present in the `resources/repository/<domainName>` directory.


### As Web Services

The SH Planning System can be deployed as a Web service. Start the Web server by running the `HTTPServer` object. By default, the server is available at `127.0.0.1:9090`. Configure the host and port in `application.conf` as needed.    

#### Available API Endpoints

| Endpoint                                      | Request Type | Body (Text)                                               |
|-----------------------------------------------|--------------|-----------------------------------------------------------|
| `services/status`                             | GET          | Check SH's status                                         |
| `services/syntax-verification/domain`         | POST         | Verify a domain definition                                |
| `services/syntax-verification/problem`        | POST         | Verify a problem definition                               |
| `services/plan-generation/domain-and-problem` | POST         | Plan with domain and problem definitions in string format |
| `services/plan-generation/problem-in-string`  | POST         | Plan with a problem definition in string format           |
| `services/storing/domain-and-problem`         | POST         | Store HPDL domain and problem definitions                 |

Note:

- For `services/planner/problem-in-string`, the domain must be already available in the repository.


## Documentation

[See SH's Wiki](https://github.com/PlanX-Universe/sh/wiki/)



## How to Cite

Georgievski, I., Palghadmal, A. V., Alnazer, E., and Aiello, M. SH: Service-oriented HTN Planning system for real-world domains. *SoftwareX*, 27: 101779. 2024.



## Relevant Literature:

1. Georgievski, I. Coordinating services embedded everywhere via hierarchical planning. Ph.D. Thesis, University of Groningen, October 2015.

2. Georgievski, I. and Aiello, M. HTN planning: Overview, comparison, and beyond. *Artificial Intelligence*, 222(0): 124-156. 2015.

3. Georgievski, I., Nguyen, T. A., Nizamic, F., Setz, B., Lazovik, A., and Aiello, M. Planning meets activity recognition: Service coordination for intelligent buildings. *Pervasive and Mobile Computing*, 38(1): 110–139. 2017.

4. Georgievski, I., Nizamic, F., Lazovik, A., and Aiello, M. Cloud Ready Applications Composed via HTN Planning. In *IEEE International Conference on Service Oriented Computing and Applications*, pages 23–33, 2017.
