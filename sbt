#!/usr/bin/env bash
set -euo pipefail

if [ ! -f project/build.properties ]; then
  echo "Run this script from the repository root." >&2
  exit 1
fi

sbt_version="$(sed -n 's/^sbt.version *= *//p' project/build.properties | tr -d '\r' | head -n 1)"
launcher_dir="${SBT_LAUNCHER_DIR:-.sbt-launcher}"
launcher="${launcher_dir}/sbt-launch-${sbt_version}.jar"

if [ -z "${sbt_version}" ]; then
  echo "Could not read sbt.version from project/build.properties." >&2
  exit 1
fi

if [ ! -f "${launcher}" ]; then
  mkdir -p "${launcher_dir}"
  tmp="${launcher}.tmp"
  curl -fsSL \
    "https://repo1.maven.org/maven2/org/scala-sbt/sbt-launch/${sbt_version}/sbt-launch-${sbt_version}.jar" \
    -o "${tmp}"
  mv "${tmp}" "${launcher}"
fi

exec java ${SBT_OPTS:-} -jar "${launcher}" "$@"
