#!/usr/bin/env bash
set -o xtrace
set -eou pipefail

export DATA=3E01030058201805141800000C313338393630373935323700006875776F2D6A743830382D65726C616E672D636C69656E7400006775657374000067756573740000026950686F6E6520334700003230312E312E312D6875776F00004F53582031300000012B3E3E002A002C20180514180000018368036400186875776F5F6A743830385F6672616D655F756E6B6E6F776E6B000578696E79696B00036C6565F03E

export DATA=7E80010005013200000003002F002E0102002D7E
export DATA=7E0102000B013200000003000C3132333435363738393041747E
cat <(echo $DATA | xxd -r -p ) - | nc localhost 8898
