public class Request {
    // declaring variables
    private final RequestType type;
    private final String accountNumber;
    private final double amount;
    private final Branch destinationBranch;
    private final String destinationAccount;

    // declaring different types of contructor (just a change in parameters)
    public Request(RequestType type, String accountNumber, double amount) {
        this(type, accountNumber, amount, null, null);
    }

    public Request(RequestType type, String accountNumber, double amount, Branch destinationBranch) {
        this(type, accountNumber, amount, destinationBranch, null);
    }

    public Request(RequestType type, String accountNumber, double amount, Branch destinationBranch,
            String destinationAccount) {
        this.type = type;
        this.accountNumber = accountNumber;
        this.amount = amount;
        this.destinationBranch = destinationBranch;
        this.destinationAccount = destinationAccount;
    }

    // returns type of request
    public RequestType getType() {
        return type;
    }

    // returns Source account number
    public String getSourceAccount() {
        return accountNumber;
    }
    
    // returns current account number
    public String getAccountNumber() {
        return accountNumber;
    }

    // returns the amount
    public double getAmount() {
        return amount;
    }

    // returns destination branch (NOTE: the whole branch element is given)
    public Branch getDestinationBranch() {
        return destinationBranch;
    }

    // returns the destination account
    public String getDestinationAccount() {
        return destinationAccount;
    }

    // tells what type of request was there
    @Override
    public String toString() {
        return "Request{" +
                "type=" + type +
                ", accountNumber='" + accountNumber + '\'' +
                ", amount=" + amount +
                (destinationBranch != null ? ", destinationBranch=" + destinationBranch.getBranchId() : "") +
                (destinationAccount != null ? ", destinationAccount='" + destinationAccount + '\'' : "") +
                '}';
    }
}
