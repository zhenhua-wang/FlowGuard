library(rsconnect)

rsconnect::setAccountInfo(
  name='zhenhua-wang-mizzou',
  token='EB426C9E39B7ABE180F65D9F7CAD3ED5',
  secret='/nMkOe+eNFd5GDHE1/lHXRlX+iRPWBmark1rw95G')

rsconnect::deployApp('~/Workspace/Hackthon/traffic_stop/')
