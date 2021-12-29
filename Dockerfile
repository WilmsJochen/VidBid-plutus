FROM inputoutput/plutus-starter-devcontainer

# Set default port
EXPOSE 9080

# Copy application code
COPY . /
USER 0

RUN cabal build plutus-starter-pab


# Start PAB server
CMD ["cabal exec -- plutus-starter-pab"]