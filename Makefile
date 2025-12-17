SCALA_MAIN_CLASS = caches.hardware.pipelined.SharedPipelinedCacheSynthTop
GENERATED_DIR = generated
QUARTUS_PROJECT_DIR = quartus/SharedPipelinedCacheSynthTop
VERILOG_FILE = SharedPipelinedCacheSynthTop.v
QUARTUS_PROJECT_NAME = SharedPipelinedCacheSynthTop

all: test

# Run the sbt tests and test if quartus can synthesize the test module
test: test-sbt test-synth

test-sbt:
	sbt test

# Test if quartus can successfully synthesis the test module
test-synth: build-hw synth

# Synthesize the test level module
synth:
	quartus_map $(QUARTUS_PROJECT_DIR)/$(QUARTUS_PROJECT_NAME)
	cat $(QUARTUS_PROJECT_DIR)/output_files/$(QUARTUS_PROJECT_NAME).map.summary
	quartus_fit $(QUARTUS_PROJECT_DIR)/$(QUARTUS_PROJECT_NAME)
	quartus_asm $(QUARTUS_PROJECT_DIR)/$(QUARTUS_PROJECT_NAME)
	quartus_sta $(QUARTUS_PROJECT_DIR)/$(QUARTUS_PROJECT_NAME)

# Build hardware
build-hw: clean-generated clean-quartus generate-verilog copy-to-quartus

# Generate Verilog for main top level module
generate-verilog:
	@echo "Generating Verilog from $(SCALA_MAIN_CLASS)..."
	@mkdir -p $(GENERATED_DIR)
	sbt "runMain $(SCALA_MAIN_CLASS)"

# Copy generated Verilog main top level module to Quartus project
copy-to-quartus: generate-verilog
	@echo "Copying $(VERILOG_FILE) to Quartus project..."
	@if [ -f "$(GENERATED_DIR)/$(VERILOG_FILE)" ]; then \
		cp "$(GENERATED_DIR)/$(VERILOG_FILE)" "$(QUARTUS_PROJECT_DIR)/$(VERILOG_FILE)"; \
		echo "Successfully copied $(VERILOG_FILE) to $(QUARTUS_PROJECT_DIR)"; \
	else \
		echo "Error: $(GENERATED_DIR)/$(VERILOG_FILE) not found!"; \
		exit 1; \
	fi

# Clean generated files
clean-generated:
	@echo "Cleaning generated files..."
	rm -rf $(GENERATED_DIR)
	rm -rf target

# Clean Quartus project files
clean-quartus:
	@echo "Cleaning Quartus project files..."
	rm -rf $(QUARTUS_PROJECT_DIR)/db
	rm -rf $(QUARTUS_PROJECT_DIR)/incremental_db
	rm -rf $(QUARTUS_PROJECT_DIR)/output_files
	rm -f $(QUARTUS_PROJECT_DIR)/*.rpt
	rm -f $(QUARTUS_PROJECT_DIR)/*.summary

# Show help
help:
	@echo "Available targets:"
	@echo "  test                           - Run sbt test and test synthesis;"
	@echo "  test-sbt                       - Run sbt test;"
	@echo "  test-synth                     - Test synthesis;"
	@echo "  synth                          - Synthesize test module in Quartus project: $(QUARTUS_PROJECT_DIR);"
	@echo "  build-hw                       - Generate Verilog for top level module and copy to Quartus project: $(QUARTUS_PROJECT_DIR);"
	@echo "  generate-verilog               - Generate Verilog for top level module: $(SCALA_MAIN_CLASS);"
	@echo "  copy-to-quartus                - Copy generated Verilog to Quartus project: $(QUARTUS_PROJECT_DIR);"
	@echo "  clean-generated                - Clean $(GENERATED_DIR) directory;"
	@echo "  clean-quartus                  - Clean Quartus project: $(QUARTUS_PROJECT_DIR);"
	@echo "  help                           - Show this help message."

# Phony targets
.PHONY: all test test-sbt test-synth synth build-hw generate-verilog copy-to-quartus clean-generated clean-quartus help
