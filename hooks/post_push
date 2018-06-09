#!/bin/bash
# hooks/post_push

# https://docs.docker.com/docker-cloud/builds/advanced/
# https://semver.org/

function add_tag() {
    echo "Adding tag ${1}"
    docker tag $IMAGE_NAME $DOCKER_REPO:$1
    docker push $DOCKER_REPO:$1
}

TAG=`git describe --tag --match "v*"`

MAJOR=`echo ${TAG} | awk -F'-' '{print $1}' | awk -F'.' '{print $1}' | sed 's/v//'`
MINOR=`echo ${TAG} | awk -F'-' '{print $1}' | awk -F'.' '{print $2}' | sed 's/v//'`
PATCH=`echo ${TAG} | awk -F'-' '{print $1}' | awk -F'.' '{print $3}' | sed 's/v//'`
PRLS=`echo ${TAG} | awk -F'-' '{print $2}'`

num='^[0-9]+$'
pre='^[0-9A-Za-z\.]+$'

echo "Current Build: ${TAG}"

if [ ! -z $MAJOR ] && [[ $MAJOR =~ $num ]]; then
    add_tag ${MAJOR}

    if [ ! -z $MINOR ] && [[ $MINOR =~ $num ]]; then
        add_tag ${MAJOR}.${MINOR}

        if [ ! -z $PATCH ] && [[ $PATCH =~ $num ]]; then
            add_tag ${MAJOR}.${MINOR}.${PATCH}

            if [ ! -z $PRLS ] && [[ ! $PRLS =~ $num ]] && [[ $PRLS =~ $pre ]]; then
                add_tag ${MAJOR}.${MINOR}.${PATCH}-${PRLS}
            fi
        fi
    fi
fi

exit $?
