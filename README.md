# The Impact of Exchange Rate Fluctuations on UK Trade





+-----------------+      +----------------+      +-----------------+      +-----------------+
| Collect Finance | ---> | Preprocess Data| ---> | Feature         | ---> | Split Data      |
| Data (API)      |      |                |      | Engineering     |      | (Train/Test)    |
+-----------------+      +----------------+      +-----------------+      +-----------------+
                                                                                        |
                                                                                        v
                                          +-----------------+      +-----------------+
                                          | Train ML Model  | ---> | Evaluate Model  |
                                          |                 |      |                 |
                                          +-----------------+      +-----------------+
                                                                                        |
                                                                                        v
                                          +-----------------+      +-----------------+
                                          | Hyperparameter  | ---> | Execute Trades  |
                                          | Tuning          |      | (Broker API)    |
                                          +-----------------+      +-----------------+
                                                                                        |
                                                                                        v
                                                 +-----------------+
                                                 | Automate &      |
                                                 | Monitor         |
                                                 +-----------------+

