if ping -q -c 1 -W 1 google.com >/dev/null; then

cd /Users/quinn/Dropbox/SBC\ LARS\ Diagnostics/Summaries/

curl -L -o grasssummaries.zip https://www.dropbox.com/sh/46i9ipvay7vk4fq/AACiSgan6RN-I0Wp1BT9_wTka/grass/summaries?dl=1

curl -L -o pinesummaries.zip https://www.dropbox.com/sh/46i9ipvay7vk4fq/AACauJIu9XRFeA4_kDko_SV4a/pine/summaries?dl=1

curl -L -o clearsummaries.zip https://www.dropbox.com/sh/46i9ipvay7vk4fq/AADjm6IUHcp1StIvK1nSMXM_a/clearcut/summaries?dl=1

unzip -a -u grasssummaries.zip
unzip -a -u pinesummaries.zip
unzip -a -u clearsummaries.zip

/usr/local/bin/Rscript --vanilla /Users/quinn/Dropbox/SBC\ LARS\ Diagnostics/LARS-Diagnostic/lars_check_up.R

rm *.zip

fi
