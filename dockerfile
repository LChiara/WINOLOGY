FROM swipl
COPY . /winology
WORKDIR /winology/src/main/prolog

CMD ["swipl", "winology.pl"]