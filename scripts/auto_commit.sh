#!/bin/sh

git pull
git add .
timestamp() {
  date +"at %H:%M:%S on %d/%m/%Y"
}
git commit -m "database auto-commit $(timestamp)"
git push