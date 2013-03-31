#!/bin/sh

date_pattern=`date "+%Y-%m-%d"`-

read -r -p "Post name > "
title=${REPLY}
clean_title=`echo $title | tr "[:upper:]" "[:lower:]"]` #Lower Case
clean_title=`echo $clean_title | iconv -f utf-8 -t ascii//translit` #Remove accents
clean_title=`echo $clean_title | tr -dc "[a-z ]"` #Keep spaces and letters
clean_title=`echo $clean_title | tr " " "-"` #Replace spaces by dashes

filename=$date_pattern$clean_title.md
author=`git config --get user.name`

cat > "posts/."$filename <<EOF
---
title: $title
author: $author
tags:
---

EOF

$EDITOR "posts/."$filename
