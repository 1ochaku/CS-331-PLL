import java.util.Random;
import java.util.concurrent.*;
import java.util.logging.Logger;

public class GNB {
    private static final Logger LOGGER = Logger.getLogger(GNB.class.getName());
    private final ConcurrentHashMap<Integer, Branch> branches;
    private final Random random;
    private final Integer initialUser = 10000;
    private final Integer numBranch = 10;

    public GNB() {
        this.branches = new ConcurrentHashMap<>();
        this.random = new Random();
        initialiseBranches();
        initialiseAccounts();
    }

    public Integer getBranchCount() {
        return numBranch;
    }

    // returns the branch corresponding to the branchId
    public Branch getBranchById(int branchId) {
        return branches.get(branchId); // Get a branch safely
    }

    // initialising the branches
    private void initialiseBranches() {
        for (int i = 0; i < numBranch; i++) {
            branches.put(i, new Branch(this, i));
        }
    }

    // initialising the intial accounts
    // using adduser function which is used only at the starting
    // TODO: if same account number already exists either here or in the branch class
    private void initialiseAccounts() {
        for (Branch branch : branches.values()) {
            for (int i = 0; i < initialUser; i++) {
                String accountNumber = generateAccountNumber(branch.getBranchId(), i);
                double initialBalance = random.nextInt(initialUser) + 1;
                branch.addUser(accountNumber, initialBalance);
            }
        }
        LOGGER.info("All accounts initialized successfully.");
    }

    // generates random account number
    public String generateAccountNumber(int branchId, int userIndex) {
        return branchId + String.format("%09d", userIndex); // 10-digit account number
    }
    
    public static void main(String[] args) {
        GNB bank = new GNB();
        // bank.initialiseBranches();
        // bank.initialiseAccounts();
        LOGGER.info("Starting transaction simulation...");
        
        // Create a thread pool for parallel branch processing
        ExecutorService executorService = Executors.newFixedThreadPool(bank.getBranchCount());

        for (int i = 0; i < bank.getBranchCount(); i++) {
            final int branchId = i;
            executorService.submit(() -> {
                Branch branch = bank.getBranchById(branchId);
                // System.out.println("\n");
                // System.out.println("Branch No: " + branchId + " executing!!!");
                branch.simulateTransactions(); // Simulate transactions for the branch
            });
        }

        // Shut down the executor service after all tasks are submitted
        executorService.shutdown();
        try {
            if (!executorService.awaitTermination(15, TimeUnit.MINUTES)) {
                executorService.shutdownNow();
            }
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            executorService.shutdownNow();
        }
    }
}

/*
 * Steps Involved are:
 * 1. Branches initialised
 * 2. All initial accounts initialised
 * 3. Each bank execute in parallel where each updater processes in parallel.
 */