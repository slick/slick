#!/bin/bash
# This script is used by travis to start docker containers. You can also use this script if you want
# to start oracle or db2 locally. It will re-use previously created containers if it finds them.
# The password in the Oracle image can expire. When this happens, you need to remove the cached image, so
# docker can create a new one: docker stop oracleslick && docker rm oracleslick: then re-run this script

if [ $# -eq 0 ]; then
  echo "Need to specify container to start. One or both of oracle, db2"
  exit 1
fi

SUCCESS_TOKEN=SUCCESS
RESULT=${SUCCESS_TOKEN}
while [ $# -ge 1 ]
do
  if [ "${1}" = "oracle" ]; then
    CONTAINER_NAMES="${CONTAINER_NAMES} oracleslick"
  elif [ "${1}" = "db2" ]; then
    CONTAINER_NAMES="${CONTAINER_NAMES} db2slick"
    DB2NAME=SLICKTST
  else
    echo "Unknown container type ${1}"
    exit 1
  fi
  shift
done

# This bit is properly weird and took me ages to find a workaround. Basically, without a connection
# to the db in a shell that is kept alive, aggregate functions with nulls behave incorrectly
# so the AggregateTest.testGroupBy test fails
db2HackConnection () {
  echo "Starting persitant db2 connection"
  docker exec -i ${CONTAINER_NAME} bash -c "su - db2inst1 -c 'while true; do db2 connect to $DB2NAME && while sleep 65535;do :; done; sleep 5; done'" &
}

# sometimes, startup on travis fails, so retry up to 5 times.
for try in 1 2 3 4 5
do
  for CONTAINER_NAME in ${CONTAINER_NAMES}
  do
    echo "Trying to start container ${CONTAINER_NAME} #$try"
    RUNNING=$(docker inspect  --format="{{ .State.Running}}" ${CONTAINER_NAME}  2> /dev/null)
    if [ $? -eq 1 ]; then
      if [ "${CONTAINER_NAME}" = "oracleslick" ]; then
	RESULT=$(docker run -d -p 49160:22 -p 49161:1521 --name ${CONTAINER_NAME} wnameless/oracle-xe-11g && echo -e "\n${SUCCESS_TOKEN}")
      elif [ "${CONTAINER_NAME}" = "db2slick" ]; then
	RESULT=$(docker run -d -p 50000:50000 --name ${CONTAINER_NAME} -e DB2INST1_PASSWORD=db2inst1-pwd -e LICENSE=accept  ibmcom/db2express-c:latest "db2start" &&
	# Extract non-free db2 jdbc driver jar
	docker cp ${CONTAINER_NAME}:/home/db2inst1/sqllib/java/db2jcc4.jar . &&
	docker exec -i -u db2inst1 -t ${CONTAINER_NAME} bash -l -c "db2 create database $DB2NAME" &&
	echo -e "\n${SUCCESS_TOKEN}")
      fi
    elif [ "$RUNNING" = "false" ]; then
      echo "Container ${CONTAINER_NAME} exists, but stopped. Starting ..."
      RESULT=$(docker start ${CONTAINER_NAME} && echo -e "\n${SUCCESS_TOKEN}")
    elif [ "$RUNNING" = "true" ]; then
      echo "Container ${CONTAINER_NAME} already running"
      RESULT=${SUCCESS_TOKEN}
    fi
    LAST_LINE=$(echo "${RESULT}" | tail -n 1)
    if [ "${LAST_LINE}" != "${SUCCESS_TOKEN}" ]; then
      echo "Container startup failed. Retry ..."
      break
    elif [ "${CONTAINER_NAME}" = "db2slick" ]; then
      db2HackConnection
    fi
  done
  if [ "${LAST_LINE}" = "${SUCCESS_TOKEN}" ]; then
    echo "Container startup succeeded"
    exit 0
  fi

# If in travis environment, restart docker service can help initalisation problems
  if [ "${TRAVIS}" = "true" ]; then
    echo "Removing container: ${CONTAINER_NAME}"
    docker stop ${CONTAINER_NAME} && docker rm ${CONTAINER_NAME}

    echo "Restart Docker:"
    sudo restart docker
    echo "Resetting iptables:"
    sudo iptables -F

    echo "Docker status:"
    sudo service docker status

    echo "Docker status:"
    sudo service docker status
  fi

  echo "Sleeping..."
  sleep 5
done 
# if we got here, container startup has failed
exit 1
