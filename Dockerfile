FROM terminusdb/terminus_store_prolog:v0.9.9.1
WORKDIR /app/terminus-store-test
COPY . .
RUN apt-get update && apt-get install time
RUN mkdir storage
CMD ./test.sh
