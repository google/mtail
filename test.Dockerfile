FROM golang:1.21

WORKDIR /app

COPY . .

# Fix go module dependencies
RUN go mod tidy

# Run the tests
CMD ["go", "test", "./..."]