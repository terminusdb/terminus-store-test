FROM terminusdb/terminus_store_prolog:v0.9.9.1
WORKDIR /app/terminus-store-test
COPY . .
RUN mkdir storage
