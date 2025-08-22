GitLab
======

## URLs

```
## https://docs.gitlab.com/ci/
## https://docs.gitlab.com/ci/runners/
## https://docs.gitlab.com/ci/variables/
## https://docs.gitlab.com/api/
## https://docs.gitlab.com/api/api_resources/
## https://docs.gitlab.com/api/rest/authentication/
```

## Creating Gitlab Runner Tags on the Registration Process

```
## https://www.bitslovers.com/gitlab-runner-tags/
## The parameter —tag-list, it’s one that we can specify
## Another crucial parameter: —run-untagged
$ sudo gitlab-runner register \
    --url "https://gitlab.www.bitslovers.com/" \
    --registration-token "THE_REGISTRATION_TOKEN" \
    --executor "docker" \
    --docker-image alpine:latest \
    --description "docker-runner" \
    --tag-list "docker,aws" \
    --run-untagged="true" \
    --locked="false"
```

## Select the Runner dynamically by Tag

```
## https://www.bitslovers.com/gitlab-runner-tags/
## Manipulate the $TAG_RUNNER value using scripts and then dynamically select
variables:
  TAG_RUNNER: aws-fargate
job:
  tags:
    - linux-large
    - $TAG_RUNNER
  script:
    - echo "Let's build our project!"
```

## Pass an environment variable to another job

```
build-job:
  stage: build
  script:
    - echo "BUILD_VARIABLE=value_from_build_job" >> build.env
  artifacts:
    reports:
      dotenv: build.env

test-job:
  stage: test
  script:
    - echo "$BUILD_VARIABLE"  # Output is: 'value_from_build_job'
```

## Artifacts always

```
## https://docs.gitlab.com/ce/ci/yaml/index.html#artifactswhen
artifacts:
  when: always
  paths:
  - SmokeTestResults/
  - package.json
```

## Gitlab Runner - Same folder (path) for each build

```
Gitlab injects environment variable CI_PROJECT_DIR with your project root directory
e.g. $CI_PROJECT_DIR/script/foo.sh
```

## Gitlab CI pipeline to run jobs parallel in same stage and invoke/trigger other jobs of same stage

```
## The key is using `needs' keyword to convert the pipieline to a directed acyclic graph:
## job1 and job2 start in parallel
## job1 will trigger the job3 (immediately, without waiting for the job2 to finish)
## job2 will trigger the job4 (immediately, without waiting for the job1 to finish)
stages:
    - stage-1
    - stage-2

job-1:
    stage: stage-1
    needs: []
    script:
      - echo "job-1 started"
      - sleep 5
      - echo "job-1 done"

job-2:
    stage: stage-1
    needs: []
    script:
      - echo "job-2 started"
      - sleep 60
      - echo "job-2 done"

job-3:
    stage: stage-2
    needs: [job-1]
    script:
      - echo "job-3 started"
      - sleep 5
      - echo "job-3 done"

job-4:
    stage: stage-2
    needs: [job-2]
    script:
      - echo "job-4 started"
      - sleep 5
      - echo "job-4 done"
```

## Missing gitlab-runner. Uploading artifacts is disabled

```
## https://docs.gitlab.com/runner/executors/ssh/
## If you want to upload job artifacts, install gitlab-runner on the host you are connecting to via SSH.
## Download the script and rename to gitlab-runner and put it into /usr/bin
## https://github.com/libstatgrab/libstatgrab-ci/blob/master/gitlab-runner-shim.sh
```

## Retrieve the status of a GitLab CI/CD pipeline using the REST API

```
$ curl --header "PRIVATE-TOKEN: <token>" \
       https://gitlab.com/api/v4/projects/<project_id>/pipelines/<pipeline_id>
$ curl https://gitlab.com/api/v4/projects/<project_id>/pipelines/<pipeline_id>?private_token=<token>
```
