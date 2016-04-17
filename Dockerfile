FROM fsharp/fsharp

COPY ./bin/SlackTG.Host ./app

EXPOSE 8083

WORKDIR ./app

CMD ["mono", "./SlackTG.Host.exe", "8083"]