# Twitter Link Processor

This is an application that listens for tweets from your fire hose and
publishes the links to delicious.

## Architecture
   

            +-----------(account_sup)-------------------+
            |                  |                        |
            |                  |                        |
    (delicous service)  (tweet translator) (twitter stream listener)

