#!/bin/sh
LOCATION=$(curl -s https://api.github.com/repos/plantuml/plantuml/releases/latest \
| grep "tag_name" \
| awk '{print "https://github.com/plantuml/plantuml/archive/" substr($2, 2, length($2)-3) ".jar"}') \
; curl -L -o plantuml.jar $LOCATION

