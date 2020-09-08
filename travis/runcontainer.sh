#!/bin/bash
# This script is used by travis to start docker containers. You can also use this script if you want
# to start oracle or db2 locally. It will re-use previously created containers if it finds them.
# The password in the Oracle image can expire. When this happens, you need to remove the cached image, so
# docker can create a new one: docker stop oracleslick && docker rm oracleslick: then re-run this script

set -x

if [ $# -eq 0 ]; then
  echo "Need to specify container to start. Any combination of oracle, db2, sqlserver"
  exit 1
fi

SUCCESS_TOKEN=SUCCESS
RESULT=${SUCCESS_TOKEN}
while [ $# -ge 1 ]
do
  if [ "${1}" = "oracle" ]; then
    CONTAINER_NAMES="${CONTAINER_NAMES} oracleslick"
  elif [ "${1}" = "sqlserver" ]; then
    CONTAINER_NAMES="${CONTAINER_NAMES} mssqlslicktest"
  elif [ "${1}" = "db2" ]; then
    CONTAINER_NAMES="${CONTAINER_NAMES} db2slick"
    DB2NAME=SLICKTST
  else
    echo "Unknown container type ${1}"
    exit 1
  fi
  shift
done

# sometimes, startup on travis fails, so retry up to 5 times.
for try in 1 2 3 4 5
do
  for CONTAINER_NAME in ${CONTAINER_NAMES}
  do
    echo "Trying to start container ${CONTAINER_NAME} #$try"
    RUNNING=$(docker inspect  --format="{{ .State.Running}}" ${CONTAINER_NAME}  2> /dev/null)
    if [ $? -eq 1 ]; then
      if [ "${CONTAINER_NAME}" = "oracleslick" ]; then
	RESULT=$(docker run -d -p 49160:22 -p 49161:1521 --name ${CONTAINER_NAME} oracleinanutshell/oracle-xe-11g && echo -e "\n${SUCCESS_TOKEN}")
      elif [ "${CONTAINER_NAME}" = "mssqlslicktest" ]; then
	RESULT=$(docker run -e 'ACCEPT_EULA=Y' -e 'MSSQL_SA_PASSWORD=Freeslick18' -p 1401:1433 --name mssqlslicktest -d microsoft/mssql-server-linux:2017-latest && echo -e "\n${SUCCESS_TOKEN}")
      elif [ "${CONTAINER_NAME}" = "db2slick" ]; then
	RESULT=$(docker run -d -p 50000:50000 --name ${CONTAINER_NAME} -e DB2INST1_PASSWORD=db2inst1-pwd -e DB2INSTANCE=db2inst1 -e DBNAME=${DB2NAME} -e LICENSE=accept --privileged=true ibmcom/db2:latest &&
	# Extract non-free db2 jdbc driver jar
	docker cp ${CONTAINER_NAME}:/opt/ibm/db2/V11.5/java/db2jcc4.jar . &&
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
