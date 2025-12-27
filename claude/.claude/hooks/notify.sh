#!/bin/bash
# 当 Claude 完成工作时发送系统通知

osascript -e 'display notification "Claude has finished working" with title "Claude Code" sound name "Glass"'
