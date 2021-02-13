FROM swipl
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    swi-prolog-x && \
    apt-get install -y x11-apps
    
RUN rm -rf /tmp/* /usr/share/doc/* /usr/share/info/* /var/tmp/*

RUN useradd -ms /bin/bash user
ENV DISPLAY :0
 
USER user

COPY . /winology
WORKDIR /winology/src/main/prolog

CMD ["swipl", "winologyGUI.pl"]
