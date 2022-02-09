_`http://localhost:8085/swagger-ui.html`_

use this website for **cron-expression**:

`https://www.freeformatter.com/cron-expression-generator-quartz.html`

Every minute:  0 * * ? * *

curl for create new job: </br>
 
` curl -X POST "http://localhost:8085/api/v1/jobs/create" -H "accept: application/json" -H "Content-Type: application/json" -d "{ \"cronExpression\": \"0 * * ? * *\", \"cronJob\": true, \"enabled\": true, \"jobClass\": \"SampleCronJob\", \"jobGroup\": \"sms\", \"jobName\": \"send-sms\"}"`