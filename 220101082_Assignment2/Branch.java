import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Random;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.locks.ReentrantLock;
import java.util.logging.Level;
import java.util.logging.Logger;

public class Branch {
    private final GNB bank; //referencing to main bank
    private final int branchId;
    private final ReentrantLock lock; // To ensure safe modifications
    private final ExecutorService updaterPool;
    private final Random random;
    private final AtomicInteger transactionCount;
    private final Integer updaterCount = 10;
    private final Integer maxTransaction = 1000;
    private final ConcurrentHashMap<String, Account> accounts;
    private static final Logger LOGGER = Logger.getLogger(GNB.class.getName());

    public Branch(GNB bank, int branchId) {
        this.bank = bank;
        this.branchId = branchId;
        this.accounts = new ConcurrentHashMap<>();
        this.lock = new ReentrantLock();
        this.updaterPool = Executors.newFixedThreadPool(updaterCount); // 10 updaters per branch
        this.random = new Random();
        this.transactionCount = new AtomicInteger(0);
    }

    // for logging the time it takes 
    public void logExecutionTime(int updaterId, long elapsedTime) {
        // Define the file to write logs to
        String logFilePath = "execution_times.txt";

        try (BufferedWriter writer = new BufferedWriter(new FileWriter(logFilePath, true))) {
            // Write execution time details to the file
            writer.write("Updater " + (this.branchId * 10 + updaterId) + " executed in " + (elapsedTime / 1_000_000_000.0) + " seconds.");
            writer.newLine();  // Add a new line after each entry
        } catch (IOException e) {
            LOGGER.severe("Error writing to the log file: " + e.getMessage());
        }
    }

    // simulate n transactions per updater
    public void simulateTransactions() {
        for (int i = 0; i < updaterCount; i++) {
            final int updaterId = i;  // Store the updater's index
            updaterPool.execute(() -> {
                long startTime = System.nanoTime();  // Record the start time for this updater

                for (int j = 0; j < maxTransaction; j++) { // Ensures each updater gets n transactions
                    // System.out.println("Updater " + updaterId + " is processing transaction " + j);
                    processRandomRequest(updaterId);
                }

                long endTime = System.nanoTime();  // Record the end time after all transactions
                long elapsedTime = endTime - startTime;  // Calculate the time taken in nanoseconds

                // Convert elapsed time to seconds and print
                // System.out.println("Updater " + (this.branchId*10 + updaterId) + " executed in " + (elapsedTime / 1_000_000_000.0) + " seconds.");
                logExecutionTime(this.branchId*10+updaterId, elapsedTime);
            });
        }

        updaterPool.shutdown(); // Stop accepting new tasks
        try {
            if (!updaterPool.awaitTermination(15, TimeUnit.MINUTES)) {
                updaterPool.shutdownNow();
            }
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
    }

    // generates random account number
    private String generateAccountNumber(int branchId, int userIndex) {
        return branchId + String.format("%09d", userIndex); // 10-digit account number
    }

    // generates random processes and hands over to process to each branch
    private void processRandomRequest(int updater) {
        double prob = random.nextDouble();
        Branch branch = bank.getBranchById(branchId);
        String accountNumber = bank.generateAccountNumber(this.branchId, random.nextInt(10_000));
        Request request = null;

        if (prob < 0.30) { // Balance Check
            request = new Request(RequestType.BALANCE_CHECK, accountNumber, 0);
        } else if (prob < 0.53) { // Deposit
            request = new Request(RequestType.DEPOSIT, accountNumber, random.nextInt(5000) + 1);
        } else if (prob < 0.76) { // Withdrawal
            request = new Request(RequestType.WITHDRAWAL, accountNumber, random.nextInt(5000) + 1);
        } else if (prob < 0.99) { // Money Transfer
            int destBranchId = random.nextInt(bank.getBranchCount());
            String destAccountNumber = generateAccountNumber(destBranchId, random.nextInt(10_000));
            Branch destBranch = bank.getBranchById(destBranchId);
            request = new Request(RequestType.MONEY_TRANSFER, accountNumber, random.nextInt(5000) + 1,
                    destBranch, destAccountNumber);
        } else if (prob < 0.993) { // Add Customer
            String newAccount = generateAccountNumber(this.branchId, random.nextInt(1_000_000));
            request = new Request(RequestType.ADD_CUSTOMER, newAccount, random.nextInt(10_000) + 1);
        } else if (prob < 0.996) { // Delete Customer
            request = new Request(RequestType.DELETE_CUSTOMER, accountNumber, 0);
        } else { // Transfer Account
            // Ensure new branch is not the same as the current branch
            int newBranchId;
            do {
                newBranchId = random.nextInt(bank.getBranchCount()); // Select random branch
            } while (newBranchId == branchId); // Make sure it's not the current branch

            Branch newBranch = bank.getBranchById(newBranchId);
            request = new Request(RequestType.TRANSFER_ACCOUNT, accountNumber, 0, newBranch);
        }

        if (request != null) {
            System.out.println("DEBUG: Updater : " + updater + " About to process request - " + request);
            branch.handleRequest(request);
            transactionCount.incrementAndGet();
            LOGGER.log(Level.FINE, "Processed request: " + request);
        }
    }

    // gives the branchId of the current branch
    public int getBranchId() {
        return branchId;
    }

    // adds user the initial time when we have to initialise
    // later ones are done using add customer
    public void addUser(String accountNumber, double initialBalance) {
        if (accounts.containsKey(accountNumber)) {
            System.out.println("Account already exists for account number: " + accountNumber);
        } else {
            accounts.put(accountNumber, new Account(accountNumber, initialBalance));
            // System.out.println("Account created successfully for account number: " + accountNumber);
        }
    }

    // when a request comes to the branch it checks which type and call the corresponding ones
    public void handleRequest(Request request) {
        switch (request.getType()) {
            case BALANCE_CHECK -> processBalanceCheck(request);
            case DEPOSIT -> processDeposit(request);
            case WITHDRAWAL -> processWithdrawal(request);
            case MONEY_TRANSFER -> processMoneyTransfer(request);
            case ADD_CUSTOMER -> processAddCustomer(request);
            case DELETE_CUSTOMER -> processDeleteCustomer(request);
            case TRANSFER_ACCOUNT -> processTransferAccount(request);
            default -> System.out.println("Invalid request type.");
        }
    }

    // we get a request to check the balance
    // we get the account number
    private void processBalanceCheck(Request request) {
        Account account = accounts.get(request.getAccountNumber());
        if (account != null) {
            System.out.println("Balance for " + request.getAccountNumber() + ": " + account.getBalance());
        } else {
            System.out.println("Account not found: " + request.getAccountNumber());
        }
    }

    // we get a request to deposit money
    // here req contain amount and account number
    // TODO: we can ask user if account doesn't exist to create an acc
    private void processDeposit(Request request) {
        Account account = accounts.get(request.getAccountNumber());
        if (account != null) {
            account.depositMoney(request.getAmount());
            System.out.println("Deposited " + request.getAmount() + " to " + request.getAccountNumber());
        } else {
            System.out.println("Account not found: " + request.getAccountNumber());
        }
    }

    // we get a request to withdraw money from the account
    // here the req contains account number and the amount to be deducted
    private void processWithdrawal(Request request) {
        Account account = accounts.get(request.getAccountNumber());
        if (account != null && account.getBalance() >= request.getAmount()) {
            account.withdrawal(request.getAmount());
            System.out.println("Withdrawn " + request.getAmount() + " from " + request.getAccountNumber());
        } else {
            System.out.println("Insufficient balance or account not found: " + request.getAccountNumber());
        }
    }

    // we get a request to transfer money
    // request contains destination branch and the destination account number, and the amount to transfer
    // here for transfering first withdrawal is checked and then depositing
    // TODO: we should unroll the transaction if it doesn't reach the destination
    private void processMoneyTransfer(Request request) {
        Branch destinationBranch = request.getDestinationBranch();

        if (destinationBranch == null) {
            System.out.println("Transfer failed: Invalid destination branch.");
            return;
        }

        Account sender = accounts.get(request.getSourceAccount());
        Account receiver = destinationBranch.getAccount(request.getDestinationAccount());

        if (sender == null || receiver == null) {
            System.out.println("Transfer failed: Invalid accounts.");
            return;
        }

        boolean success = sender.transferMoney(receiver, request.getAmount());
        if (!success) {
            System.out.println("Money transfer operation failed for account number: " + sender.getAccountNumber());
        } else {
            System.out.println("Money transfer successful for account number: " + sender.getAccountNumber() + " to "
                    + receiver.getAccountNumber());
        }
    }

    // we get a request to add a customer and we process it
    // request contains acc no and amount
    private void processAddCustomer(Request request) {
        String accountNumber = request.getAccountNumber();
        if (!accounts.containsKey(accountNumber)) {
            accounts.put(accountNumber, new Account(accountNumber, request.getAmount()));
            System.out.println("Added new customer: " + accountNumber);
        } else {
            System.out.println("Account already exists: " + accountNumber);
        }
    }

    // we get a request to delete the account and we process it
    // request contains acc no
    private void processDeleteCustomer(Request request) {
        if (accounts.remove(request.getAccountNumber()) != null) {
            System.out.println("Deleted account: " + request.getAccountNumber());
        } else {
            System.out.println("Account not found: " + request.getAccountNumber());
        }
    }

    // we get a request to transfer account to another branch
    // using addUser function: we can merge both add user and processAddCustomer
    // THIS NEEDS TO BE IN BANK LEVEL
    private void processTransferAccount(Request request) {
        lock.lock();
        try {
            Account account = accounts.remove(request.getAccountNumber());
            if (account != null) {
                request.getDestinationBranch().addUser(account.getAccountNumber(), account.getBalance());
                System.out.println("Transferred account " + request.getAccountNumber() + " to another branch.");
            } else {
                System.out.println("Account not found for transfer: " + request.getAccountNumber());
            }
        } finally {
            lock.unlock();
        }
    }

    // return the account related to the associated acc num
    public Account getAccount(String accountNumber) {
        return accounts.get(accountNumber);
    }
}

