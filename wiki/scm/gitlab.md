GitLab
======

## URLs

```
## https://docs.gitlab.com/ci/variables/
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
